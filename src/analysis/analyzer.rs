use crate::parser::ast::AstNode;
use crate::parser::token::Span;

use super::symbol_table::{SymbolInfo, SymbolKind, SymbolReference, SymbolTable};

pub fn analyze(nodes: &[AstNode]) -> SymbolTable {
    let mut table = SymbolTable::new();
    for node in nodes {
        analyze_node(node, &mut table);
    }
    table
}

fn analyze_node(node: &AstNode, table: &mut SymbolTable) {
    match node {
        AstNode::Defun {
            name,
            name_span,
            params,
            locals,
            body,
            doc_comment,
            span,
        } => {
            // Register function definition
            table.add_definition(SymbolInfo {
                name: name.clone(),
                kind: SymbolKind::Function,
                span: *span,
                name_span: *name_span,
                params: params.iter().map(|(n, _)| n.clone()).collect(),
                locals: locals.iter().map(|(n, _)| n.clone()).collect(),
                doc_comment: doc_comment.clone(),
            });

            // Register function name as definition reference
            table.add_reference(SymbolReference {
                name: name.clone(),
                span: *name_span,
                is_definition: true,
            });

            // Register parameters as definitions
            for (param_name, param_span) in params {
                table.add_definition(SymbolInfo {
                    name: param_name.clone(),
                    kind: SymbolKind::Parameter,
                    span: *param_span,
                    name_span: *param_span,
                    params: vec![],
                    locals: vec![],
                    doc_comment: None,
                });
                table.add_reference(SymbolReference {
                    name: param_name.clone(),
                    span: *param_span,
                    is_definition: true,
                });
            }

            // Register local variables as definitions
            for (local_name, local_span) in locals {
                table.add_definition(SymbolInfo {
                    name: local_name.clone(),
                    kind: SymbolKind::LocalVar,
                    span: *local_span,
                    name_span: *local_span,
                    params: vec![],
                    locals: vec![],
                    doc_comment: None,
                });
                table.add_reference(SymbolReference {
                    name: local_name.clone(),
                    span: *local_span,
                    is_definition: true,
                });
            }

            // Analyze body
            for expr in body {
                analyze_node(expr, table);
            }
        }

        AstNode::Lambda { params, body, .. } => {
            for (param_name, param_span) in params {
                table.add_definition(SymbolInfo {
                    name: param_name.clone(),
                    kind: SymbolKind::Parameter,
                    span: *param_span,
                    name_span: *param_span,
                    params: vec![],
                    locals: vec![],
                    doc_comment: None,
                });
                table.add_reference(SymbolReference {
                    name: param_name.clone(),
                    span: *param_span,
                    is_definition: true,
                });
            }
            for expr in body {
                analyze_node(expr, table);
            }
        }

        AstNode::List(elements, _) => {
            analyze_list(elements, table);
        }

        AstNode::Symbol(name, span) => {
            table.add_reference(SymbolReference {
                name: name.clone(),
                span: *span,
                is_definition: false,
            });
        }

        AstNode::Quote(inner, _) => {
            // Don't analyze quoted expressions as references
            // (they're data, not code)
            analyze_quoted(inner, table);
        }

        AstNode::DottedPair(car, cdr, _) => {
            analyze_node(car, table);
            analyze_node(cdr, table);
        }

        // Literals and errors don't contribute to symbol table
        AstNode::IntegerLit(_, _)
        | AstNode::RealLit(_, _)
        | AstNode::StringLit(_, _)
        | AstNode::Nil(_)
        | AstNode::T(_)
        | AstNode::Comment(_, _)
        | AstNode::Error(_, _) => {}
    }
}

fn analyze_list(elements: &[AstNode], table: &mut SymbolTable) {
    if elements.is_empty() {
        return;
    }

    // Check for special forms
    if let AstNode::Symbol(name, _) = &elements[0] {
        match name.as_str() {
            "SETQ" => {
                analyze_setq(&elements[1..], table);
                // Still register SETQ as a reference
                analyze_node(&elements[0], table);
                return;
            }
            "FOREACH" => {
                analyze_foreach(&elements[1..], table);
                analyze_node(&elements[0], table);
                return;
            }
            _ => {}
        }
    }

    // Regular list: analyze all elements
    for element in elements {
        analyze_node(element, table);
    }
}

fn analyze_setq(args: &[AstNode], table: &mut SymbolTable) {
    // setq takes pairs: (setq var1 val1 var2 val2 ...)
    let mut i = 0;
    while i + 1 < args.len() {
        if let AstNode::Symbol(name, span) = &args[i] {
            // Register as variable definition
            table.add_definition(SymbolInfo {
                name: name.clone(),
                kind: SymbolKind::Variable,
                span: *span,
                name_span: *span,
                params: vec![],
                locals: vec![],
                doc_comment: None,
            });
            table.add_reference(SymbolReference {
                name: name.clone(),
                span: *span,
                is_definition: true,
            });
        }
        // Analyze the value expression
        analyze_node(&args[i + 1], table);
        i += 2;
    }
}

fn analyze_foreach(args: &[AstNode], table: &mut SymbolTable) {
    // (foreach var list-expr body...)
    if args.is_empty() {
        return;
    }

    if let AstNode::Symbol(name, span) = &args[0] {
        table.add_definition(SymbolInfo {
            name: name.clone(),
            kind: SymbolKind::Variable,
            span: *span,
            name_span: *span,
            params: vec![],
            locals: vec![],
            doc_comment: None,
        });
        table.add_reference(SymbolReference {
            name: name.clone(),
            span: *span,
            is_definition: true,
        });
    }

    for arg in &args[1..] {
        analyze_node(arg, table);
    }
}

fn analyze_quoted(_node: &AstNode, _table: &mut SymbolTable) {
    // Quoted expressions are data, not code
    // We don't register references for symbols inside quoted forms
}

/// Find the function call context at a given offset.
/// Returns (function_name, argument_index) if the cursor is inside a function call.
pub fn find_function_call_at(nodes: &[AstNode], offset: usize) -> Option<(String, usize)> {
    for node in nodes {
        if let Some(result) = find_call_in_node(node, offset) {
            return Some(result);
        }
    }
    None
}

fn find_call_in_node(node: &AstNode, offset: usize) -> Option<(String, usize)> {
    let span = node.span();
    if offset < span.start || offset > span.end {
        return None;
    }

    match node {
        AstNode::List(elements, _) => {
            // First check deeper nesting
            for element in elements.iter().skip(1) {
                if let Some(result) = find_call_in_node(element, offset) {
                    return Some(result);
                }
            }

            // If not in a deeper call, check if we're in this call
            if let Some(AstNode::Symbol(name, _)) = elements.first() {
                // Calculate which argument the cursor is at
                let arg_index = elements
                    .iter()
                    .skip(1) // skip function name
                    .take_while(|e| e.span().end <= offset)
                    .count();
                return Some((name.clone(), arg_index));
            }
            None
        }

        AstNode::Defun { body, .. } => {
            for expr in body {
                if let Some(result) = find_call_in_node(expr, offset) {
                    return Some(result);
                }
            }
            None
        }

        AstNode::Lambda { body, .. } => {
            for expr in body {
                if let Some(result) = find_call_in_node(expr, offset) {
                    return Some(result);
                }
            }
            None
        }

        _ => None,
    }
}

/// Collect all parse errors from the AST
pub fn collect_errors(nodes: &[AstNode]) -> Vec<(String, Span)> {
    let mut errors = Vec::new();
    for node in nodes {
        collect_errors_in_node(node, &mut errors);
    }
    errors
}

fn collect_errors_in_node(node: &AstNode, errors: &mut Vec<(String, Span)>) {
    match node {
        AstNode::Error(msg, span) => {
            errors.push((msg.clone(), *span));
        }
        AstNode::List(elements, _) => {
            for e in elements {
                collect_errors_in_node(e, errors);
            }
        }
        AstNode::Defun { body, .. } | AstNode::Lambda { body, .. } => {
            for e in body {
                collect_errors_in_node(e, errors);
            }
        }
        AstNode::Quote(inner, _) => {
            collect_errors_in_node(inner, errors);
        }
        AstNode::DottedPair(car, cdr, _) => {
            collect_errors_in_node(car, errors);
            collect_errors_in_node(cdr, errors);
        }
        _ => {}
    }
}
