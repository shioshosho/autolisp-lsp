use tower_lsp::lsp_types::*;

use crate::config::FormatConfig;
use crate::document::Document;
use crate::parser::ast::AstNode;
use crate::parser::token::Span;

const INDENT_SIZE: usize = 2;
const LINE_LIMIT: usize = 100;

// Special forms that get extra indentation for their body
const SPECIAL_FORMS: &[&str] = &[
    "DEFUN", "DEFUN-Q", "LAMBDA", "IF", "COND", "WHILE", "REPEAT", "FOREACH", "PROGN",
    "VL-CATCH-ALL-APPLY",
];

/// Returns the number of distinguished arguments for a special form.
/// Distinguished args are placed on the same line as the form name.
fn distinguished_args_count(name: &str) -> usize {
    match name {
        "IF" | "WHILE" | "REPEAT" | "VL-CATCH-ALL-APPLY" => 1,
        "FOREACH" => 2,
        "COND" | "PROGN" => 0,
        _ => 0,
    }
}

/// Resolve the display name for a symbol.
/// When `force_convert_case` is true, returns `name.to_lowercase()`.
/// When false, extracts the original text from the source using the span.
fn format_symbol_name(original: &str, span: Span, name: &str, config: &FormatConfig) -> String {
    if config.force_convert_case {
        name.to_lowercase()
    } else {
        original[span.start..span.end].to_string()
    }
}

/// Format a node into a temporary string for width measurement.
fn format_node_to_string(
    node: &AstNode,
    indent: usize,
    comments: &[AstNode],
    original: &str,
    config: &FormatConfig,
) -> String {
    let mut buf = String::new();
    format_node(node, indent, comments, &mut buf, original, config);
    buf
}

pub fn format_document(
    doc: &Document,
    _options: &FormattingOptions,
    config: &FormatConfig,
) -> Vec<TextEdit> {
    let formatted = format_nodes(&doc.ast, &doc.comments, &doc.text, config);
    if formatted == doc.text {
        return vec![];
    }

    let start = Position::new(0, 0);
    let end = doc.offset_to_position(doc.text.len());

    vec![TextEdit {
        range: Range { start, end },
        new_text: formatted,
    }]
}

pub fn format_range(
    doc: &Document,
    range: &Range,
    _options: &FormattingOptions,
    config: &FormatConfig,
) -> Vec<TextEdit> {
    let start_offset = doc.position_to_offset(range.start);
    let end_offset = doc.position_to_offset(range.end);

    // Find the top-level nodes that overlap the range
    let mut nodes_in_range: Vec<&AstNode> = Vec::new();
    for node in &doc.ast {
        let span = node.span();
        if span.end > start_offset && span.start < end_offset {
            nodes_in_range.push(node);
        }
    }

    if nodes_in_range.is_empty() {
        return vec![];
    }

    // Format just the nodes in range
    let first_span = nodes_in_range.first().unwrap().span();
    let last_span = nodes_in_range.last().unwrap().span();

    let mut output = String::new();
    for (i, node) in nodes_in_range.iter().enumerate() {
        if i > 0 {
            output.push_str("\n\n");
        }
        format_node(node, 0, &doc.comments, &mut output, &doc.text, config);
    }
    output.push('\n');

    let start = doc.offset_to_position(first_span.start);
    let end = doc.offset_to_position(last_span.end);

    // Check if there's a newline after the last node
    let actual_end = if last_span.end < doc.text.len()
        && doc.text.as_bytes().get(last_span.end) == Some(&b'\n')
    {
        doc.offset_to_position(last_span.end + 1)
    } else {
        end
    };

    vec![TextEdit {
        range: Range {
            start,
            end: actual_end,
        },
        new_text: output,
    }]
}

/// Filter comments whose start falls within the given span range [start, end).
fn comments_in_range<'a>(comments: &'a [AstNode], start: usize, end: usize) -> Vec<&'a AstNode> {
    comments
        .iter()
        .filter(|c| {
            let cs = c.span().start;
            cs >= start && cs < end
        })
        .collect()
}

fn format_nodes(
    nodes: &[AstNode],
    comments: &[AstNode],
    original: &str,
    config: &FormatConfig,
) -> String {
    let mut output = String::new();

    // Collect all top-level items (nodes + top-level comments) sorted by position
    let mut items: Vec<FormattedItem> = Vec::new();

    for node in nodes {
        items.push(FormattedItem::Node(node));
    }
    // Only include top-level comments (those not inside any AST node's span).
    // Comments inside defun/lambda bodies are handled by format_body_with_comments.
    for comment in comments {
        let cs = comment.span().start;
        let inside_node = nodes.iter().any(|n| {
            let s = n.span();
            cs >= s.start && cs < s.end
        });
        if !inside_node {
            items.push(FormattedItem::Comment(comment));
        }
    }

    items.sort_by_key(|item| match item {
        FormattedItem::Node(n) => n.span().start,
        FormattedItem::Comment(c) => c.span().start,
    });

    let mut prev_end: Option<usize> = None;

    for item in &items {
        match item {
            FormattedItem::Node(node) => {
                // Add blank line between top-level forms
                if prev_end.is_some() {
                    output.push_str("\n\n");
                }
                format_node(node, 0, comments, &mut output, original, config);
                prev_end = Some(node.span().end);
            }
            FormattedItem::Comment(comment) => {
                if let Some(pe) = prev_end {
                    // Check if comment was on the same line in original
                    let between = &original[pe..comment.span().start];
                    if !between.contains('\n') {
                        output.push(' ');
                    } else {
                        output.push('\n');
                    }
                }
                format_comment(comment, &mut output);
                prev_end = Some(comment.span().end);
            }
        }
    }

    if !output.is_empty() && !output.ends_with('\n') {
        output.push('\n');
    }

    output
}

enum FormattedItem<'a> {
    Node(&'a AstNode),
    Comment(&'a AstNode),
}

fn format_comment(node: &AstNode, output: &mut String) {
    if let AstNode::Comment(text, _) = node {
        output.push(';');
        output.push_str(text);
    }
}

fn format_node(
    node: &AstNode,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    match node {
        AstNode::IntegerLit(v, _) => {
            output.push_str(&v.to_string());
        }
        AstNode::RealLit(v, _) => {
            output.push_str(&format_real(*v));
        }
        AstNode::StringLit(s, _) => {
            output.push('"');
            for ch in s.chars() {
                match ch {
                    '"' => output.push_str("\\\""),
                    '\\' => output.push_str("\\\\"),
                    '\n' => output.push_str("\\n"),
                    '\r' => output.push_str("\\r"),
                    '\t' => output.push_str("\\t"),
                    _ => output.push(ch),
                }
            }
            output.push('"');
        }
        AstNode::Symbol(name, span) => {
            output.push_str(&format_symbol_name(original, *span, name, config));
        }
        AstNode::Nil(_) => output.push_str("nil"),
        AstNode::T(_) => output.push('T'),
        AstNode::Quote(inner, _) => {
            output.push('\'');
            format_node(inner, indent, comments, output, original, config);
        }
        AstNode::DottedPair(car, cdr, _) => {
            output.push('(');
            format_node(car, indent + 1, comments, output, original, config);
            output.push_str(" . ");
            format_node(cdr, indent + 1, comments, output, original, config);
            output.push(')');
        }
        AstNode::Defun {
            name,
            name_span,
            params,
            locals,
            body,
            doc_comment,
            span,
        } => {
            format_defun(
                name, *name_span, params, locals, body, doc_comment, *span, indent, comments,
                output, original, config,
            );
        }
        AstNode::Lambda { params, body, span } => {
            format_lambda(params, body, *span, indent, comments, output, original, config);
        }
        AstNode::List(elements, span) => {
            format_list(elements, *span, indent, comments, output, original, config);
        }
        AstNode::Comment(text, _) => {
            output.push(';');
            output.push_str(text);
        }
        AstNode::Error(msg, _) => {
            // Preserve errors as comments
            output.push_str(&format!("; ERROR: {}", msg));
        }
    }
}

/// Format a sequence of body expressions, interleaving any comments that fall
/// between them. Each element is placed on its own line at the given indent.
/// Returns `true` if the last emitted item was a comment, `false` if it was an expression.
fn format_body_with_comments(
    elements: &[AstNode],
    parent_span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) -> bool {
    let body_comments = comments_in_range(comments, parent_span.start, parent_span.end);
    let mut last_was_comment = false;

    if elements.is_empty() {
        // Only comments in the body
        for c in &body_comments {
            output.push('\n');
            push_indent(output, indent);
            format_comment(c, output);
            last_was_comment = true;
        }
        return last_was_comment;
    }

    // Track which comments have been emitted
    let mut comment_idx = 0;

    for (i, expr) in elements.iter().enumerate() {
        let expr_start = expr.span().start;

        // Emit comments that come before this expression
        while comment_idx < body_comments.len()
            && body_comments[comment_idx].span().start < expr_start
        {
            output.push('\n');
            push_indent(output, indent);
            format_comment(body_comments[comment_idx], output);
            comment_idx += 1;
            last_was_comment = true;
        }

        // Skip comments that overlap with the expression (already inside it)
        while comment_idx < body_comments.len()
            && body_comments[comment_idx].span().start < expr.span().end
        {
            comment_idx += 1;
        }

        output.push('\n');
        push_indent(output, indent);
        format_node(expr, indent, comments, output, original, config);
        last_was_comment = false;

        // Check for inline comment on same line after expression
        // (comment that starts right after expression, before next newline)
        if comment_idx < body_comments.len() {
            let next_elem_start = if i + 1 < elements.len() {
                elements[i + 1].span().start
            } else {
                parent_span.end
            };
            let cc = body_comments[comment_idx];
            let cs = cc.span().start;
            if cs >= expr.span().end && cs < next_elem_start {
                // Check if there are no body expressions between this comment
                // and the current one, suggesting it's an inline comment
                // We just emit it on the next line for simplicity
            }
        }
    }

    // Emit trailing comments after the last expression
    while comment_idx < body_comments.len() {
        output.push('\n');
        push_indent(output, indent);
        format_comment(body_comments[comment_idx], output);
        comment_idx += 1;
        last_was_comment = true;
    }

    last_was_comment
}

fn format_defun(
    name: &str,
    name_span: Span,
    params: &[(String, Span)],
    locals: &[(String, Span)],
    body: &[AstNode],
    doc_comment: &Option<String>,
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    output.push_str("(defun ");
    output.push_str(&format_symbol_name(original, name_span, name, config));
    output.push_str(" (");

    // Parameters
    for (i, (p, p_span)) in params.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&format_symbol_name(original, *p_span, p, config));
    }

    // Local variables
    if !locals.is_empty() {
        if !params.is_empty() {
            output.push(' ');
        }
        output.push('/');
        for (l, l_span) in locals {
            output.push(' ');
            output.push_str(&format_symbol_name(original, *l_span, l, config));
        }
    }

    output.push(')');

    let body_indent = indent + INDENT_SIZE;

    // Doc comment
    if let Some(doc) = doc_comment {
        output.push('\n');
        push_indent(output, body_indent);
        output.push('"');
        output.push_str(doc);
        output.push('"');
    }

    // Body with interleaved comments
    // Determine the range for body comments: after param list closing paren to defun closing paren.
    // Use the end of the last param/local as the lower bound so that comments between
    // the param list and first body expression are included.
    let after_params = locals
        .last()
        .map(|(_, s)| s.end)
        .or_else(|| params.last().map(|(_, s)| s.end))
        .unwrap_or(span.start);
    let body_span = Span::new(after_params, span.end);

    if body.is_empty() && doc_comment.is_none() {
        // Empty body: stack closing paren right after param list
        let body_comments = comments_in_range(comments, body_span.start, body_span.end);
        if body_comments.is_empty() {
            output.push(')');
            return;
        }
    }

    let ends_with_comment =
        format_body_with_comments(body, body_span, body_indent, comments, output, original, config);
    if ends_with_comment {
        output.push('\n');
        push_indent(output, indent);
    }
    output.push(')');
}

fn format_lambda(
    params: &[(String, Span)],
    body: &[AstNode],
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    output.push_str("(lambda (");

    for (i, (p, p_span)) in params.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&format_symbol_name(original, *p_span, p, config));
    }

    output.push(')');

    let body_indent = indent + INDENT_SIZE;
    let has_comments = !comments_in_range(comments, span.start, span.end).is_empty();

    if body.len() == 1 && is_simple_expr(&body[0]) && !has_comments {
        output.push(' ');
        format_node(&body[0], body_indent, comments, output, original, config);
        output.push(')');
    } else {
        let body_start = body.first().map(|b| b.span().start).unwrap_or(span.start);
        let body_span = Span::new(body_start, span.end);
        let ends_with_comment =
            format_body_with_comments(body, body_span, body_indent, comments, output, original, config);
        if ends_with_comment {
            output.push('\n');
            push_indent(output, indent);
        }
        output.push(')');
    }
}

fn format_list(
    elements: &[AstNode],
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    if elements.is_empty() {
        output.push_str("()");
        return;
    }

    // Check if this is a special form
    let is_special = if let AstNode::Symbol(name, _) = &elements[0] {
        SPECIAL_FORMS.contains(&name.as_str())
    } else {
        false
    };

    let has_comments = !comments_in_range(comments, span.start, span.end).is_empty();

    // Try single-line format first (only if no comments inside)
    if !has_comments {
        let single_line = format_list_single_line(elements, indent, comments, original, config);
        if single_line.len() + indent <= LINE_LIMIT && !single_line.contains('\n') {
            output.push_str(&single_line);
            return;
        }
    }

    // Multi-line format: dispatch to special form or regular call
    if is_special {
        if let AstNode::Symbol(name, _) = &elements[0] {
            format_list_special_form(name, elements, span, indent, comments, output, original, config);
        }
    } else {
        format_list_regular_call(elements, span, indent, comments, output, original, config);
    }
}

/// Format a special form list in multi-line mode.
/// Distinguished args go on the same line as the form name,
/// remaining body args get indent+2 on separate lines.
fn format_list_special_form(
    name: &str,
    elements: &[AstNode],
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    let dist_count = distinguished_args_count(name);
    let body_indent = indent + INDENT_SIZE;

    output.push('(');
    format_node(&elements[0], indent + 1, comments, output, original, config);

    // Output distinguished args on the same line
    let dist_end = std::cmp::min(1 + dist_count, elements.len());
    for element in &elements[1..dist_end] {
        output.push(' ');
        format_node(element, indent + 1, comments, output, original, config);
    }

    // Remaining body args on new lines
    if dist_end < elements.len() {
        let body_elements = &elements[dist_end..];
        let body_start = body_elements.first().map(|b| b.span().start).unwrap_or(span.start);
        let body_span = Span::new(body_start, span.end);
        let ends_with_comment =
            format_body_with_comments(body_elements, body_span, body_indent, comments, output, original, config);
        if ends_with_comment {
            output.push('\n');
            push_indent(output, indent);
        }
    }
    output.push(')');
}

/// Format a regular function call in multi-line mode.
/// First arg on same line, remaining args aligned to first arg position.
fn format_list_regular_call(
    elements: &[AstNode],
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
    original: &str,
    config: &FormatConfig,
) {
    output.push('(');
    format_node(&elements[0], indent + 1, comments, output, original, config);

    if elements.len() == 1 {
        output.push(')');
        return;
    }

    // Calculate alignment column: (func-name + space
    let func_str = format_node_to_string(&elements[0], indent + 1, comments, original, config);
    let align_col = indent + 1 + func_str.len() + 1; // ( + name + space

    // Fallback to indent+2 if alignment would be too deep
    let align_col = if align_col > 40 { indent + INDENT_SIZE } else { align_col };

    // First arg on same line
    output.push(' ');
    format_node(&elements[1], align_col, comments, output, original, config);

    // Remaining args aligned
    if elements.len() > 2 {
        let list_comments = comments_in_range(comments, span.start, span.end);
        let mut comment_idx = 0;
        let mut last_was_comment = false;

        // Skip comments before or inside elements[0] and elements[1]
        let skip_until = elements[1].span().end;
        while comment_idx < list_comments.len()
            && list_comments[comment_idx].span().start < skip_until
        {
            comment_idx += 1;
        }

        for element in &elements[2..] {
            let elem_start = element.span().start;

            // Emit comments before this element
            while comment_idx < list_comments.len()
                && list_comments[comment_idx].span().start < elem_start
            {
                let cs = list_comments[comment_idx].span().start;
                let inside_sibling = elements.iter().any(|e| {
                    let es = e.span();
                    cs >= es.start && cs < es.end
                });
                if !inside_sibling {
                    output.push('\n');
                    push_indent(output, align_col);
                    format_comment(list_comments[comment_idx], output);
                    last_was_comment = true;
                }
                comment_idx += 1;
            }

            // Skip comments inside this element
            while comment_idx < list_comments.len()
                && list_comments[comment_idx].span().start < element.span().end
            {
                comment_idx += 1;
            }

            output.push('\n');
            push_indent(output, align_col);
            format_node(element, align_col, comments, output, original, config);
            last_was_comment = false;
        }

        // Trailing comments
        while comment_idx < list_comments.len() {
            let cs = list_comments[comment_idx].span().start;
            let inside_child = elements.iter().any(|e| {
                let es = e.span();
                cs >= es.start && cs < es.end
            });
            if !inside_child {
                output.push('\n');
                push_indent(output, align_col);
                format_comment(list_comments[comment_idx], output);
                last_was_comment = true;
            }
            comment_idx += 1;
        }

        if last_was_comment {
            output.push('\n');
            push_indent(output, indent);
        }
    }
    output.push(')');
}

fn format_list_single_line(
    elements: &[AstNode],
    indent: usize,
    comments: &[AstNode],
    original: &str,
    config: &FormatConfig,
) -> String {
    let mut out = String::new();
    out.push('(');
    for (i, element) in elements.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        format_node(element, indent + 1, comments, &mut out, original, config);
    }
    out.push(')');
    out
}

fn is_simple_expr(node: &AstNode) -> bool {
    match node {
        AstNode::IntegerLit(_, _)
        | AstNode::RealLit(_, _)
        | AstNode::StringLit(_, _)
        | AstNode::Symbol(_, _)
        | AstNode::Nil(_)
        | AstNode::T(_) => true,
        AstNode::List(elements, _) => elements.len() <= 3 && elements.iter().all(is_simple_expr),
        _ => false,
    }
}

fn format_real(v: f64) -> String {
    let s = v.to_string();
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

fn push_indent(output: &mut String, indent: usize) {
    for _ in 0..indent {
        output.push(' ');
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;

    #[test]
    fn test_format_japanese_string() {
        let input = "(setq ver \"DRCエラーラベル変更コマンド\")\n";
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        if edits.is_empty() {
            // No changes means input is already correct
            assert!(input.contains("DRCエラーラベル変更コマンド"));
        } else {
            let formatted = &edits[0].new_text;
            assert!(
                formatted.contains("\"DRCエラーラベル変更コマンド\""),
                "Japanese string corrupted: {}",
                formatted
            );
        }
    }

    #[test]
    fn test_format_japanese_comment() {
        let input = "; DRCエラーラベル変更コマンド\n(+ 1 2)\n";
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        if edits.is_empty() {
            assert!(input.contains("DRCエラーラベル変更コマンド"));
        } else {
            let formatted = &edits[0].new_text;
            assert!(
                formatted.contains("DRCエラーラベル変更コマンド"),
                "Japanese comment corrupted: {}",
                formatted
            );
        }
    }

    #[test]
    fn test_format_japanese_in_defun() {
        let input = "(defun test (x)\n  ; 日本語コメント\n  (setq msg \"日本語テスト\")\n  msg\n)\n";
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        if edits.is_empty() {
            assert!(input.contains("日本語コメント"));
            assert!(input.contains("日本語テスト"));
        } else {
            let formatted = &edits[0].new_text;
            assert!(
                formatted.contains("日本語コメント"),
                "Japanese comment in defun corrupted: {}",
                formatted
            );
            assert!(
                formatted.contains("\"日本語テスト\""),
                "Japanese string in defun corrupted: {}",
                formatted
            );
        }
    }

    #[test]
    fn test_format_crlf_strips_cr_from_comments() {
        let input = "; 日本語コメント\r\n(+ 1 2)\r\n";
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        // CRLF input should produce LF-only output
        assert!(!edits.is_empty(), "CRLF should trigger formatting changes");
        let formatted = &edits[0].new_text;
        assert!(
            !formatted.contains('\r'),
            "Output should not contain CR: {:?}",
            formatted
        );
        assert!(
            formatted.contains("日本語コメント"),
            "Japanese comment should be preserved: {}",
            formatted
        );
    }

    #[test]
    fn test_format_crlf_defun_comments() {
        let input = "(defun test (x)\r\n  ; 文字位置インデックス\r\n  (+ x 1)\r\n)\r\n";
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        assert!(!edits.is_empty(), "CRLF should trigger formatting changes");
        let formatted = &edits[0].new_text;
        assert!(
            !formatted.contains('\r'),
            "Output should not contain CR: {:?}",
            formatted
        );
        assert!(
            formatted.contains("文字位置インデックス"),
            "Japanese comment should be preserved: {}",
            formatted
        );
    }

    /// Helper to format input and return the result
    fn fmt(input: &str) -> String {
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), &FormatConfig::default());
        if edits.is_empty() {
            input.to_string()
        } else {
            edits[0].new_text.clone()
        }
    }

    /// Helper to format input with a given config and return the result
    fn fmt_with_config(input: &str, config: &FormatConfig) -> String {
        let doc = Document::new(input.to_string());
        let edits = format_document(&doc, &FormattingOptions::default(), config);
        if edits.is_empty() {
            input.to_string()
        } else {
            edits[0].new_text.clone()
        }
    }

    #[test]
    fn test_format_defun_stacked_parens() {
        let input = "(defun test (x / temp)\n  (setq msg \"hello\")\n  (alert msg)\n)\n";
        let result = fmt(input);
        assert_eq!(
            result,
            "(defun test (x / temp)\n  (setq msg \"hello\")\n  (alert msg))\n",
            "Closing paren should stack on last body expression"
        );
    }

    #[test]
    fn test_format_if_with_progn() {
        // Each sub-expression must exceed LINE_LIMIT when combined with indent
        let input = "(if (= some-long-variable-name another-long-variable-name)\n  (progn\n    (setq result-value-variable \"some-long-initial-value\")\n    (alert \"operation completed successfully with message\"))\n  (alert \"operation failed with an error message to display\"))\n";
        let result = fmt(input);
        let expected = "(if (= some-long-variable-name another-long-variable-name)\n  (progn\n    (setq result-value-variable \"some-long-initial-value\")\n    (alert \"operation completed successfully with message\"))\n  (alert \"operation failed with an error message to display\"))\n";
        assert_eq!(
            result, expected,
            "if+progn closing parens should stack"
        );
    }

    #[test]
    fn test_format_cond() {
        // Make cond body long enough to force multi-line
        let input = "(cond\n  ((= long-variable-name 1) \"result-for-condition-one\")\n  ((= long-variable-name 2) \"result-for-condition-two\")\n  (T \"default-result-for-other-cases\"))\n";
        let result = fmt(input);
        let expected = "(cond\n  ((= long-variable-name 1) \"result-for-condition-one\")\n  ((= long-variable-name 2) \"result-for-condition-two\")\n  (T \"default-result-for-other-cases\"))\n";
        assert_eq!(
            result, expected,
            "cond should have 0 distinguished args and stacked closing paren"
        );
    }

    #[test]
    fn test_format_foreach() {
        // Make foreach long enough to force multi-line
        let input = "(foreach current-item some-long-list-variable-name\n  (print current-item)\n  (setq counter (+ counter 1)))\n";
        let result = fmt(input);
        let expected = "(foreach current-item some-long-list-variable-name\n  (print current-item)\n  (setq counter (+ counter 1)))\n";
        assert_eq!(
            result, expected,
            "foreach should have 2 distinguished args (var, list) on same line"
        );
    }

    #[test]
    fn test_format_regular_call_alignment() {
        // strcat with args that exceed line limit to force multi-line (>100 chars total)
        let input = "(strcat \"hello-this-is-a-very-long-string-value\" \" separator-string \" \"world-another-very-long-string-value\")\n";
        let result = fmt(input);
        // Should align args to first arg position: (strcat <-- here
        // strcat = 6 chars, so align_col = 1 + 6 + 1 = 8
        assert!(
            result.contains("\n        \""),
            "Regular call args should align to first arg position (col 8): {}",
            result
        );
    }

    #[test]
    fn test_format_nested_stacking() {
        // if body must exceed LINE_LIMIT at indent=2 to force multi-line
        let input = "(defun calculate-result (first-value-param second-value-param)\n  (if (> first-value-param second-value-param)\n    (- first-value-param second-value-param)\n    (+ first-value-param second-value-param)))\n";
        let result = fmt(input);
        let expected = "(defun calculate-result (first-value-param second-value-param)\n  (if (> first-value-param second-value-param)\n    (- first-value-param second-value-param)\n    (+ first-value-param second-value-param)))\n";
        assert_eq!(
            result, expected,
            "Nested closing parens should all stack"
        );
    }

    #[test]
    fn test_format_no_closing_paren_alone() {
        let inputs = [
            "(defun test (x)\n  (+ x 1)\n)\n",
            // Long enough inputs that force multi-line
            "(defun my-long-function-name (some-parameter / local-var)\n  (setq local-var (+ some-parameter 1))\n  (alert (itoa local-var))\n)\n",
        ];
        for input in &inputs {
            let result = fmt(input);
            for line in result.lines() {
                let trimmed = line.trim();
                assert!(
                    trimmed != ")" && trimmed != "))" && trimmed != ")))",
                    "No line should contain only closing parens.\nInput: {}\nOutput: {}\nBad line: '{}'",
                    input, result, line
                );
            }
        }
    }

    #[test]
    fn test_format_preserve_case() {
        let config = FormatConfig { force_convert_case: false };
        let input = "(defun MyFunc (Param1 Param2 / LocalVar)\n  (setq LocalVar (+ Param1 Param2))\n  LocalVar)\n";
        let result = fmt_with_config(input, &config);
        assert!(
            result.contains("MyFunc"),
            "Function name should preserve original case: {}",
            result
        );
        assert!(
            result.contains("Param1"),
            "Parameter should preserve original case: {}",
            result
        );
        assert!(
            result.contains("LocalVar"),
            "Local variable should preserve original case: {}",
            result
        );
    }

    #[test]
    fn test_format_force_lowercase() {
        let config = FormatConfig { force_convert_case: true };
        let input = "(defun MyFunc (Param1 Param2 / LocalVar)\n  (setq LocalVar (+ Param1 Param2))\n  LocalVar)\n";
        let result = fmt_with_config(input, &config);
        assert!(
            result.contains("myfunc"),
            "Function name should be lowercased: {}",
            result
        );
        assert!(
            result.contains("param1"),
            "Parameter should be lowercased: {}",
            result
        );
        assert!(
            result.contains("localvar"),
            "Local variable should be lowercased: {}",
            result
        );
        assert!(
            !result.contains("MyFunc"),
            "Original case should not appear: {}",
            result
        );
    }

    #[test]
    fn test_format_preserve_case_lambda() {
        let config = FormatConfig { force_convert_case: false };
        let input = "(lambda (MyParam) (+ MyParam 1))\n";
        let result = fmt_with_config(input, &config);
        assert!(
            result.contains("MyParam"),
            "Lambda parameter should preserve original case: {}",
            result
        );
    }
}
