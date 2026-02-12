use tower_lsp::lsp_types::*;

use crate::document::Document;
use crate::parser::ast::AstNode;
use crate::parser::token::Span;

const INDENT_SIZE: usize = 2;

// Special forms that get extra indentation for their body
const SPECIAL_FORMS: &[&str] = &[
    "DEFUN", "DEFUN-Q", "LAMBDA", "IF", "COND", "WHILE", "REPEAT", "FOREACH", "PROGN",
    "VL-CATCH-ALL-APPLY",
];

pub fn format_document(doc: &Document, _options: &FormattingOptions) -> Vec<TextEdit> {
    let formatted = format_nodes(&doc.ast, &doc.comments, &doc.text);
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

pub fn format_range(doc: &Document, range: &Range, _options: &FormattingOptions) -> Vec<TextEdit> {
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
        format_node(node, 0, &doc.comments, &mut output);
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

fn format_nodes(nodes: &[AstNode], comments: &[AstNode], original: &str) -> String {
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
                format_node(node, 0, comments, &mut output);
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

fn format_node(node: &AstNode, indent: usize, comments: &[AstNode], output: &mut String) {
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
        AstNode::Symbol(name, _) => {
            output.push_str(&name.to_lowercase());
        }
        AstNode::Nil(_) => output.push_str("nil"),
        AstNode::T(_) => output.push('T'),
        AstNode::Quote(inner, _) => {
            output.push('\'');
            format_node(inner, indent, comments, output);
        }
        AstNode::DottedPair(car, cdr, _) => {
            output.push('(');
            format_node(car, indent + 1, comments, output);
            output.push_str(" . ");
            format_node(cdr, indent + 1, comments, output);
            output.push(')');
        }
        AstNode::Defun {
            name,
            params,
            locals,
            body,
            doc_comment,
            span,
            ..
        } => {
            format_defun(name, params, locals, body, doc_comment, *span, indent, comments, output);
        }
        AstNode::Lambda { params, body, span } => {
            format_lambda(params, body, *span, indent, comments, output);
        }
        AstNode::List(elements, span) => {
            format_list(elements, *span, indent, comments, output);
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
fn format_body_with_comments(
    elements: &[AstNode],
    parent_span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
) {
    let body_comments = comments_in_range(comments, parent_span.start, parent_span.end);

    if elements.is_empty() {
        // Only comments in the body
        for c in &body_comments {
            output.push('\n');
            push_indent(output, indent);
            format_comment(c, output);
        }
        return;
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
        }

        // Skip comments that overlap with the expression (already inside it)
        while comment_idx < body_comments.len()
            && body_comments[comment_idx].span().start < expr.span().end
        {
            comment_idx += 1;
        }

        output.push('\n');
        push_indent(output, indent);
        format_node(expr, indent, comments, output);

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
    }
}

fn format_defun(
    name: &str,
    params: &[(String, Span)],
    locals: &[(String, Span)],
    body: &[AstNode],
    doc_comment: &Option<String>,
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
) {
    output.push_str("(defun ");
    output.push_str(&name.to_lowercase());
    output.push_str(" (");

    // Parameters
    for (i, (p, _)) in params.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&p.to_lowercase());
    }

    // Local variables
    if !locals.is_empty() {
        if !params.is_empty() {
            output.push(' ');
        }
        output.push('/');
        for (l, _) in locals {
            output.push(' ');
            output.push_str(&l.to_lowercase());
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

    format_body_with_comments(body, body_span, body_indent, comments, output);

    output.push('\n');
    push_indent(output, indent);
    output.push(')');
}

fn format_lambda(
    params: &[(String, Span)],
    body: &[AstNode],
    span: Span,
    indent: usize,
    comments: &[AstNode],
    output: &mut String,
) {
    output.push_str("(lambda (");

    for (i, (p, _)) in params.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&p.to_lowercase());
    }

    output.push(')');

    let body_indent = indent + INDENT_SIZE;
    let has_comments = !comments_in_range(comments, span.start, span.end).is_empty();

    if body.len() == 1 && is_simple_expr(&body[0]) && !has_comments {
        output.push(' ');
        format_node(&body[0], body_indent, comments, output);
        output.push(')');
    } else {
        let body_start = body.first().map(|b| b.span().start).unwrap_or(span.start);
        let body_span = Span::new(body_start, span.end);
        format_body_with_comments(body, body_span, body_indent, comments, output);
        output.push('\n');
        push_indent(output, indent);
        output.push(')');
    }
}

fn format_list(elements: &[AstNode], span: Span, indent: usize, comments: &[AstNode], output: &mut String) {
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
        let single_line = format_list_single_line(elements, indent, comments);
        if single_line.len() + indent <= 80 && !single_line.contains('\n') {
            output.push_str(&single_line);
            return;
        }
    }

    // Multi-line format
    output.push('(');
    format_node(&elements[0], indent + 1, comments, output);

    let body_indent = if is_special {
        indent + INDENT_SIZE
    } else {
        indent + INDENT_SIZE
    };

    if elements.len() > 1 {
        // Get comments that belong inside this list but not inside child nodes
        let list_comments = comments_in_range(comments, span.start, span.end);
        let mut comment_idx = 0;

        // Skip comments before or inside the first element
        while comment_idx < list_comments.len()
            && list_comments[comment_idx].span().start < elements[0].span().end
        {
            comment_idx += 1;
        }

        for (i, element) in elements[1..].iter().enumerate() {
            let elem_start = element.span().start;

            // Emit comments before this element
            while comment_idx < list_comments.len()
                && list_comments[comment_idx].span().start < elem_start
            {
                // Only emit if not inside a sibling element
                let cs = list_comments[comment_idx].span().start;
                let inside_sibling = elements.iter().any(|e| {
                    let es = e.span();
                    cs >= es.start && cs < es.end
                });
                if !inside_sibling {
                    output.push('\n');
                    push_indent(output, body_indent);
                    format_comment(list_comments[comment_idx], output);
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
            push_indent(output, body_indent);
            format_node(element, body_indent, comments, output);
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
                push_indent(output, body_indent);
                format_comment(list_comments[comment_idx], output);
            }
            comment_idx += 1;
        }
    }

    output.push('\n');
    push_indent(output, indent);
    output.push(')');
}

fn format_list_single_line(elements: &[AstNode], indent: usize, comments: &[AstNode]) -> String {
    let mut out = String::new();
    out.push('(');
    for (i, element) in elements.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        format_node(element, indent + 1, comments, &mut out);
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
