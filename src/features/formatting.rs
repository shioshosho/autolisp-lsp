use tower_lsp::lsp_types::*;

use crate::document::Document;
use crate::parser::ast::AstNode;

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
        format_node(node, 0, &mut output);
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

fn format_nodes(nodes: &[AstNode], comments: &[AstNode], original: &str) -> String {
    let mut output = String::new();

    // Collect all top-level items (nodes + comments) sorted by position
    let mut items: Vec<FormattedItem> = Vec::new();

    for node in nodes {
        items.push(FormattedItem::Node(node));
    }
    for comment in comments {
        items.push(FormattedItem::Comment(comment));
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
                format_node(node, 0, &mut output);
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
    match node {
        AstNode::Comment(text, _) => {
            // Determine if it's a block comment based on content
            // Block comments in our AST were parsed from ;| ... |;
            // For now, just output as line comment
            output.push(';');
            output.push_str(text);
        }
        _ => {}
    }
}

fn format_node(node: &AstNode, indent: usize, output: &mut String) {
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
            format_node(inner, indent, output);
        }
        AstNode::DottedPair(car, cdr, _) => {
            output.push('(');
            format_node(car, indent + 1, output);
            output.push_str(" . ");
            format_node(cdr, indent + 1, output);
            output.push(')');
        }
        AstNode::Defun {
            name,
            params,
            locals,
            body,
            doc_comment,
            ..
        } => {
            format_defun(name, params, locals, body, doc_comment, indent, output);
        }
        AstNode::Lambda { params, body, .. } => {
            format_lambda(params, body, indent, output);
        }
        AstNode::List(elements, _) => {
            format_list(elements, indent, output);
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

fn format_defun(
    name: &str,
    params: &[(String, crate::parser::token::Span)],
    locals: &[(String, crate::parser::token::Span)],
    body: &[AstNode],
    doc_comment: &Option<String>,
    indent: usize,
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

    // Body
    for expr in body {
        output.push('\n');
        push_indent(output, body_indent);
        format_node(expr, body_indent, output);
    }

    output.push('\n');
    push_indent(output, indent);
    output.push(')');
}

fn format_lambda(
    params: &[(String, crate::parser::token::Span)],
    body: &[AstNode],
    indent: usize,
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

    if body.len() == 1 && is_simple_expr(&body[0]) {
        output.push(' ');
        format_node(&body[0], body_indent, output);
        output.push(')');
    } else {
        for expr in body {
            output.push('\n');
            push_indent(output, body_indent);
            format_node(expr, body_indent, output);
        }
        output.push('\n');
        push_indent(output, indent);
        output.push(')');
    }
}

fn format_list(elements: &[AstNode], indent: usize, output: &mut String) {
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

    // Try single-line format first
    let single_line = format_list_single_line(elements, indent);
    if single_line.len() + indent <= 80 && !single_line.contains('\n') {
        output.push_str(&single_line);
        return;
    }

    // Multi-line format
    output.push('(');
    format_node(&elements[0], indent + 1, output);

    let body_indent = if is_special {
        indent + INDENT_SIZE
    } else {
        // Align with first argument
        indent + INDENT_SIZE
    };

    for element in &elements[1..] {
        output.push('\n');
        push_indent(output, body_indent);
        format_node(element, body_indent, output);
    }

    output.push('\n');
    push_indent(output, indent);
    output.push(')');
}

fn format_list_single_line(elements: &[AstNode], indent: usize) -> String {
    let mut out = String::new();
    out.push('(');
    for (i, element) in elements.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        format_node(element, indent + 1, &mut out);
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
