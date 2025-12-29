//! Matrix and vector operations library.
//!
//! Provides operations on vectors and matrices:
//! - →V2, →V3, V→: Vector construction/destruction
//! - AXL: Array/list conversion
//! - MADD, MSUB, HADAMARD: Element-wise operations
//! - DOT, CROSS: Vector products
//! - MSCALE: Scalar multiplication
//! - CON: Constant array
//! - TRAN: Transpose

use std::sync::OnceLock;

use rpl::interface::InterfaceSpec;

use rpl::{
    core::Span,
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Matrix library.
const INTERFACE: &str = include_str!("interfaces/matrix.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid matrix interface"))
}

/// Matrix library ID (matches newRPL lib-52).
pub const MATRIX_LIB: LibId = 52;

/// Matrix library command IDs.
pub mod cmd {
    /// Make 2-element vector (→V2).
    pub const TO_V2: u16 = 0;
    /// Make 3-element vector (→V3).
    pub const TO_V3: u16 = 1;
    /// Explode vector (V→).
    pub const V_TO: u16 = 2;
    /// Array/list conversion (AXL).
    pub const AXL: u16 = 3;
    /// Element-wise addition (MADD).
    pub const MADD: u16 = 4;
    /// Element-wise subtraction (MSUB).
    pub const MSUB: u16 = 5;
    /// Element-wise multiplication (HADAMARD).
    pub const HADAMARD: u16 = 6;
    /// Element-wise negation (MNEG).
    pub const MNEG: u16 = 7;
    /// Scalar multiplication (MSCALE).
    pub const MSCALE: u16 = 8;
    /// Dot product (DOT).
    pub const DOT: u16 = 9;
    /// Cross product (CROSS).
    pub const CROSS: u16 = 10;
    /// Get dimensions (MDIM).
    pub const MDIM: u16 = 11;
    /// Frobenius norm / vector magnitude (ABS).
    pub const ABS: u16 = 12;
    /// Constant array (CON).
    pub const CON: u16 = 13;
    /// Transpose (TRAN).
    pub const TRAN: u16 = 14;
}

/// Matrix operations library (implementation only).
#[derive(Clone, Copy)]
pub struct MatrixLib;

impl LibraryLowerer for MatrixLib {
    fn id(&self) -> LibId {
        MATRIX_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(MATRIX_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for MatrixLib {
    fn id(&self) -> LibId {
        MATRIX_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::TO_V2 => {
                // →V2: (x y -- [x y])
                let y = ctx.pop()?;
                let x = ctx.pop()?;
                ctx.push(Value::vector(vec![x, y]))?;
                Ok(ExecuteAction::ok())
            }

            cmd::TO_V3 => {
                // →V3: (x y z -- [x y z])
                let z = ctx.pop()?;
                let y = ctx.pop()?;
                let x = ctx.pop()?;
                ctx.push(Value::vector(vec![x, y, z]))?;
                Ok(ExecuteAction::ok())
            }

            cmd::V_TO => {
                // V→: ([elements] -- elements... n)
                let vec = ctx.pop()?;
                let n = match &vec {
                    Value::Matrix(m) => {
                        for item in m.data.iter() {
                            ctx.push(item.clone())?;
                        }
                        m.data.len()
                    }
                    Value::List(items) => {
                        for item in items.iter() {
                            ctx.push(item.clone())?;
                        }
                        items.len()
                    }
                    _ => return Err(format!("V→: expected matrix/list, got {}", vec.type_name())),
                };
                ctx.push(Value::Integer(n as i64))?;
                Ok(ExecuteAction::ok())
            }

            cmd::AXL => {
                // AXL: convert between matrix and nested list
                let val = ctx.pop()?;
                let result = match &val {
                    Value::Matrix(m) => matrix_to_list(m),
                    Value::List(_) => list_to_matrix(&val)?,
                    _ => return Err(format!("AXL: expected matrix or list, got {}", val.type_name())),
                };
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::MADD => {
                // MADD: element-wise addition
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                let result = matrix_elementwise_binop(&a, &b, "MADD", &|x, y| add_values(x, y))?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::MSUB => {
                // MSUB: element-wise subtraction
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                let result = matrix_elementwise_binop(&a, &b, "MSUB", &|x, y| sub_values(x, y))?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::HADAMARD => {
                // HADAMARD: element-wise multiplication
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                let result = matrix_elementwise_binop(&a, &b, "HADAMARD", &|x, y| mul_values(x, y))?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::MNEG => {
                // MNEG: element-wise negation
                let a = ctx.pop()?;
                let result = matrix_elementwise_unop(&a, "MNEG", &neg_value)?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::MSCALE => {
                // MSCALE: scalar multiplication (matrix/list scalar -- matrix/list)
                let scalar = ctx.pop()?;
                let vec = ctx.pop()?;
                let result = matrix_elementwise_unop(&vec, "MSCALE", &|x| mul_values(x, &scalar))?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::DOT => {
                // DOT: dot product (sum of element-wise products)
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                let (a_slice, b_slice): (&[Value], &[Value]) = match (&a, &b) {
                    (Value::Matrix(ma), Value::Matrix(mb)) => (&ma.data, &mb.data),
                    (Value::List(la), Value::List(lb)) => (la.as_ref(), lb.as_ref()),
                    (Value::Matrix(m), Value::List(l)) => (&m.data, l.as_ref()),
                    (Value::List(l), Value::Matrix(m)) => (l.as_ref(), &m.data),
                    _ => return Err(format!("DOT: expected matrix/list, got {} and {}", a.type_name(), b.type_name())),
                };
                if a_slice.len() != b_slice.len() {
                    return Err(format!("DOT: dimension mismatch ({} vs {})", a_slice.len(), b_slice.len()));
                }
                let mut sum = 0.0;
                for (x, y) in a_slice.iter().zip(b_slice.iter()) {
                    let product = mul_values(x, y)?;
                    sum += to_real(&product)?;
                }
                ctx.push(Value::Real(sum))?;
                Ok(ExecuteAction::ok())
            }

            cmd::CROSS => {
                // CROSS: 3D cross product
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                let (a_slice, b_slice): (&[Value], &[Value]) = match (&a, &b) {
                    (Value::Matrix(ma), Value::Matrix(mb)) => (&ma.data, &mb.data),
                    (Value::List(la), Value::List(lb)) => (la.as_ref(), lb.as_ref()),
                    (Value::Matrix(m), Value::List(l)) => (&m.data, l.as_ref()),
                    (Value::List(l), Value::Matrix(m)) => (l.as_ref(), &m.data),
                    _ => return Err(format!("CROSS: expected matrix/list, got {} and {}", a.type_name(), b.type_name())),
                };
                if a_slice.len() != 3 || b_slice.len() != 3 {
                    return Err("CROSS: requires 3D vectors".into());
                }
                let av: Vec<f64> = a_slice.iter().map(to_real).collect::<Result<_, _>>()?;
                let bv: Vec<f64> = b_slice.iter().map(to_real).collect::<Result<_, _>>()?;
                let result = vec![
                    Value::Real(av[1] * bv[2] - av[2] * bv[1]),
                    Value::Real(av[2] * bv[0] - av[0] * bv[2]),
                    Value::Real(av[0] * bv[1] - av[1] * bv[0]),
                ];
                ctx.push(Value::vector(result))?;
                Ok(ExecuteAction::ok())
            }

            cmd::MDIM => {
                // MDIM: get dimensions as list
                let val = ctx.pop()?;
                let dims = match &val {
                    Value::Matrix(m) => vec![m.rows as i64, m.cols as i64],
                    Value::List(items) => {
                        // For nested lists, compute dimensions
                        let dims = get_list_dimensions(items)?;
                        dims.into_iter().map(|d| d as i64).collect()
                    }
                    _ => return Err(format!("MDIM: expected matrix/list, got {}", val.type_name())),
                };
                let dims_values: Vec<Value> = dims.into_iter().map(Value::Integer).collect();
                ctx.push(Value::list(dims_values))?;
                Ok(ExecuteAction::ok())
            }

            cmd::ABS => {
                // ABS: Frobenius norm (magnitude for vectors), or numeric absolute value
                let val = ctx.pop()?;
                match &val {
                    Value::Integer(i) => {
                        ctx.push(Value::Integer(i.abs()))?;
                    }
                    Value::Real(r) => {
                        ctx.push(Value::Real(r.abs()))?;
                    }
                    Value::Matrix(m) => {
                        let mut sum_sq = 0.0;
                        for item in m.data.iter() {
                            let v = to_real(item)?;
                            sum_sq += v * v;
                        }
                        ctx.push(Value::Real(sum_sq.sqrt()))?;
                    }
                    Value::List(items) => {
                        let mut sum_sq = 0.0;
                        for item in items.iter() {
                            let v = to_real(item)?;
                            sum_sq += v * v;
                        }
                        ctx.push(Value::Real(sum_sq.sqrt()))?;
                    }
                    _ => return Err(format!("ABS: expected number/matrix/list, got {}", val.type_name())),
                }
                Ok(ExecuteAction::ok())
            }

            cmd::CON => {
                // CON: constant array ({dims} value -- matrix)
                let value = ctx.pop()?;
                let dims = ctx.pop()?;
                let dims_list = match &dims {
                    Value::List(d) => d,
                    _ => return Err(format!("CON: expected dimension list, got {}", dims.type_name())),
                };
                let result = make_constant_matrix(dims_list, &value)?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::TRAN => {
                // TRAN: transpose (for 2D matrices)
                let arr = ctx.pop()?;
                let result = transpose(&arr)?;
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            _ => Err(format!("Unknown matrix command: {}", ctx.cmd)),
        }
    }
}

// Helper functions

fn to_real(v: &Value) -> Result<f64, String> {
    match v {
        Value::Integer(i) => Ok(*i as f64),
        Value::Real(r) => Ok(*r),
        _ => Err(format!("expected number, got {}", v.type_name())),
    }
}

fn add_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
        _ => Err(format!("cannot add {} and {}", a.type_name(), b.type_name())),
    }
}

fn sub_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
        _ => Err(format!("cannot subtract {} and {}", a.type_name(), b.type_name())),
    }
}

fn mul_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        _ => Err(format!("cannot multiply {} and {}", a.type_name(), b.type_name())),
    }
}

fn neg_value(a: &Value) -> Result<Value, String> {
    match a {
        Value::Integer(a) => Ok(Value::Integer(-a)),
        Value::Real(a) => Ok(Value::Real(-a)),
        _ => Err(format!("cannot negate {}", a.type_name())),
    }
}

/// Element-wise binary operation on matrices or lists.
fn matrix_elementwise_binop(
    a: &Value,
    b: &Value,
    cmd: &str,
    op: &dyn Fn(&Value, &Value) -> Result<Value, String>,
) -> Result<Value, String> {
    match (a, b) {
        // Matrix × Matrix
        (Value::Matrix(ma), Value::Matrix(mb)) => {
            if ma.rows != mb.rows || ma.cols != mb.cols {
                return Err(format!(
                    "{}: dimension mismatch ({}×{} vs {}×{})",
                    cmd, ma.rows, ma.cols, mb.rows, mb.cols
                ));
            }
            let mut result = Vec::with_capacity(ma.data.len());
            for (x, y) in ma.data.iter().zip(mb.data.iter()) {
                result.push(op(x, y)?);
            }
            Ok(Value::matrix(ma.rows, ma.cols, result))
        }
        // List × List (for backwards compatibility)
        (Value::List(a_items), Value::List(b_items)) => {
            if a_items.len() != b_items.len() {
                return Err(format!(
                    "{}: dimension mismatch ({} vs {})",
                    cmd,
                    a_items.len(),
                    b_items.len()
                ));
            }
            let mut result = Vec::with_capacity(a_items.len());
            for (x, y) in a_items.iter().zip(b_items.iter()) {
                result.push(matrix_elementwise_binop(x, y, cmd, op)?);
            }
            Ok(Value::list(result))
        }
        _ => op(a, b),
    }
}

/// Element-wise unary operation on matrices or lists.
fn matrix_elementwise_unop(
    a: &Value,
    cmd: &str,
    op: &dyn Fn(&Value) -> Result<Value, String>,
) -> Result<Value, String> {
    match a {
        Value::Matrix(m) => {
            let mut result = Vec::with_capacity(m.data.len());
            for x in m.data.iter() {
                result.push(op(x)?);
            }
            Ok(Value::matrix(m.rows, m.cols, result))
        }
        Value::List(items) => {
            let mut result = Vec::with_capacity(items.len());
            for x in items.iter() {
                result.push(matrix_elementwise_unop(x, cmd, op)?);
            }
            Ok(Value::list(result))
        }
        _ => op(a),
    }
}

/// Get dimensions of nested list (for backwards compatibility).
fn get_list_dimensions(items: &[Value]) -> Result<Vec<usize>, String> {
    if items.is_empty() {
        return Ok(vec![0]);
    }
    // Check if all items are lists of same length (matrix)
    let first = &items[0];
    if let Value::List(first_row) = first {
        let row_len = first_row.len();
        for item in items.iter().skip(1) {
            if let Value::List(row) = item {
                if row.len() != row_len {
                    return Err("irregular matrix dimensions".into());
                }
            } else {
                // Mixed content, treat as 1D
                return Ok(vec![items.len()]);
            }
        }
        // All rows same length, it's a matrix
        Ok(vec![items.len(), row_len])
    } else {
        // 1D vector
        Ok(vec![items.len()])
    }
}

/// Create a constant matrix with given dimensions.
fn make_constant_matrix(dims: &[Value], value: &Value) -> Result<Value, String> {
    if dims.is_empty() {
        return Ok(value.clone());
    }

    let first = match &dims[0] {
        Value::Integer(n) if *n >= 0 => *n as u16,
        _ => return Err("CON: dimension must be non-negative integer".into()),
    };

    if dims.len() == 1 {
        // 1D vector
        Ok(Value::vector(vec![value.clone(); first as usize]))
    } else if dims.len() == 2 {
        // 2D matrix
        let cols = match &dims[1] {
            Value::Integer(n) if *n >= 0 => *n as u16,
            _ => return Err("CON: dimension must be non-negative integer".into()),
        };
        let data = vec![value.clone(); (first as usize) * (cols as usize)];
        Ok(Value::matrix(first, cols, data))
    } else {
        // Higher dimensions not supported for Matrix type, fall back to nested lists
        let inner = make_constant_matrix(&dims[1..], value)?;
        Ok(Value::list(vec![inner; first as usize]))
    }
}

/// Convert Matrix to nested list.
fn matrix_to_list(m: &rpl::value::MatrixData) -> Value {
    if m.rows == 0 || m.rows == 1 {
        // Vector: just return as flat list
        Value::list(m.data.to_vec())
    } else {
        // 2D matrix: create nested lists
        let mut rows = Vec::with_capacity(m.rows as usize);
        for r in 0..m.rows {
            let start = (r as usize) * (m.cols as usize);
            let end = start + (m.cols as usize);
            rows.push(Value::list(m.data[start..end].to_vec()));
        }
        Value::list(rows)
    }
}

/// Convert nested list to Matrix.
fn list_to_matrix(val: &Value) -> Result<Value, String> {
    let items = match val {
        Value::List(items) => items,
        _ => return Err(format!("AXL: expected list, got {}", val.type_name())),
    };

    if items.is_empty() {
        return Ok(Value::vector(vec![]));
    }

    // Check if it's nested (2D matrix)
    if let Value::List(first_row) = &items[0] {
        let cols = first_row.len() as u16;
        let mut data = Vec::with_capacity(items.len() * (cols as usize));
        for row in items.iter() {
            if let Value::List(r) = row {
                if r.len() != cols as usize {
                    return Err("AXL: irregular matrix dimensions".into());
                }
                data.extend(r.iter().cloned());
            } else {
                return Err("AXL: expected 2D nested list".into());
            }
        }
        Ok(Value::matrix(items.len() as u16, cols, data))
    } else {
        // 1D vector
        Ok(Value::vector(items.to_vec()))
    }
}

/// Transpose a matrix or nested list.
fn transpose(arr: &Value) -> Result<Value, String> {
    match arr {
        Value::Matrix(m) => {
            let rows = m.rows as usize;
            let cols = m.cols as usize;
            let mut result = Vec::with_capacity(m.data.len());
            for c in 0..cols {
                for r in 0..rows {
                    result.push(m.data[r * cols + c].clone());
                }
            }
            Ok(Value::matrix(m.cols, m.rows, result))
        }
        Value::List(items) => {
            if items.is_empty() {
                return Ok(arr.clone());
            }
            // Check if it's a 2D matrix
            let first_row = match &items[0] {
                Value::List(r) => r,
                _ => {
                    // 1D vector: treat as row vector, transpose to column matrix
                    let data: Vec<Value> = items.to_vec();
                    return Ok(Value::matrix(items.len() as u16, 1, data));
                }
            };

            let n_rows = items.len() as u16;
            let n_cols = first_row.len() as u16;

            // Verify all rows have same length
            for row in items.iter() {
                if let Value::List(r) = row {
                    if r.len() != n_cols as usize {
                        return Err("TRAN: irregular matrix".into());
                    }
                } else {
                    return Err("TRAN: expected 2D matrix".into());
                }
            }

            // Transpose to Matrix type
            let mut result = Vec::with_capacity((n_rows as usize) * (n_cols as usize));
            for j in 0..n_cols {
                for row in items.iter() {
                    if let Value::List(r) = row {
                        result.push(r[j as usize].clone());
                    }
                }
            }
            Ok(Value::matrix(n_cols, n_rows, result))
        }
        _ => Err(format!("TRAN: expected matrix/list, got {}", arr.type_name())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matrix_lib_id() {
        assert_eq!(interface().id(), 52);
    }

    #[test]
    fn matrix_lib_name() {
        assert_eq!(interface().name(), "Matrix");
    }

    #[test]
    fn to_v2() {
        let result = crate::eval("1 2 →V2").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(1), Value::Integer(2)]));
    }

    #[test]
    fn to_v3() {
        let result = crate::eval("1 2 3 →V3").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
    }

    #[test]
    fn v_to_list() {
        // V→ works on lists
        let result = crate::eval("{ 1 2 3 } V→").unwrap();
        assert_eq!(result.len(), 4); // 3 elements + count
        assert_eq!(result[0], Value::Integer(1));
        assert_eq!(result[1], Value::Integer(2));
        assert_eq!(result[2], Value::Integer(3));
        assert_eq!(result[3], Value::Integer(3));
    }

    #[test]
    fn v_to_matrix() {
        // V→ works on vectors (matrices)
        let result = crate::eval("1 2 3 →V3 V→").unwrap();
        assert_eq!(result.len(), 4); // 3 elements + count
        assert_eq!(result[0], Value::Integer(1));
        assert_eq!(result[1], Value::Integer(2));
        assert_eq!(result[2], Value::Integer(3));
        assert_eq!(result[3], Value::Integer(3));
    }

    #[test]
    fn madd_lists() {
        // MADD works on lists for backwards compatibility
        let result = crate::eval("{ 1 2 3 } { 4 5 6 } MADD").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::list(vec![Value::Integer(5), Value::Integer(7), Value::Integer(9)]));
    }

    #[test]
    fn madd_matrices() {
        // MADD works on matrices
        let result = crate::eval("1 2 3 →V3 4 5 6 →V3 MADD").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(5), Value::Integer(7), Value::Integer(9)]));
    }

    #[test]
    fn msub() {
        let result = crate::eval("5 7 9 →V3 1 2 3 →V3 MSUB").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(4), Value::Integer(5), Value::Integer(6)]));
    }

    #[test]
    fn hadamard() {
        let result = crate::eval("2 3 4 →V3 5 6 7 →V3 HADAMARD").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(10), Value::Integer(18), Value::Integer(28)]));
    }

    #[test]
    fn dot_product() {
        let result = crate::eval("1 2 3 →V3 4 5 6 →V3 DOT").unwrap();
        assert_eq!(result.len(), 1);
        // 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        assert_eq!(result[0], Value::Real(32.0));
    }

    #[test]
    fn cross_product() {
        // i x j = k
        let result = crate::eval("1 0 0 →V3 0 1 0 →V3 CROSS").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Real(0.0), Value::Real(0.0), Value::Real(1.0)]));
    }

    #[test]
    fn vector_magnitude() {
        // |[3, 4]| = 5
        let result = crate::eval("3 4 →V2 ABS").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::Real(5.0));
    }

    #[test]
    fn constant_vector() {
        let result = crate::eval("{ 3 } 0 CON").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(0), Value::Integer(0), Value::Integer(0)]));
    }

    #[test]
    fn constant_matrix() {
        let result = crate::eval("{ 2 3 } 1 CON").unwrap();
        assert_eq!(result.len(), 1);
        // 2x3 matrix filled with 1s
        assert_eq!(result[0], Value::matrix(2, 3, vec![
            Value::Integer(1), Value::Integer(1), Value::Integer(1),
            Value::Integer(1), Value::Integer(1), Value::Integer(1),
        ]));
    }

    #[test]
    fn mscale() {
        let result = crate::eval("1 2 3 →V3 2 MSCALE").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(2), Value::Integer(4), Value::Integer(6)]));
    }

    #[test]
    fn axl_list_to_matrix() {
        // Convert list to matrix
        let result = crate::eval("{ 1 2 3 } AXL").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
    }

    #[test]
    fn axl_matrix_to_list() {
        // Convert matrix to list
        let result = crate::eval("1 2 3 →V3 AXL").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::list(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
    }

    #[test]
    fn transpose_matrix() {
        // Transpose a 2x3 matrix to 3x2
        let result = crate::eval("{ 2 3 } 0 CON TRAN").unwrap();
        assert_eq!(result.len(), 1);
        // 3x2 matrix
        assert_eq!(result[0], Value::matrix(3, 2, vec![
            Value::Integer(0), Value::Integer(0),
            Value::Integer(0), Value::Integer(0),
            Value::Integer(0), Value::Integer(0),
        ]));
    }

    #[test]
    fn mdim_matrix() {
        let result = crate::eval("{ 2 3 } 0 CON MDIM").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::list(vec![Value::Integer(2), Value::Integer(3)]));
    }

    #[test]
    fn matrix_plus_operator() {
        // Test that + does element-wise addition on matrices
        let result = crate::eval("1 2 →V2 3 4 →V2 +").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(4), Value::Integer(6)]));
    }

    #[test]
    fn matrix_minus_operator() {
        // Test that - does element-wise subtraction on matrices
        let result = crate::eval("5 6 →V2 3 4 →V2 -").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(2), Value::Integer(2)]));
    }

    #[test]
    fn matrix_times_scalar() {
        // Test that * with scalar scales the matrix
        let result = crate::eval("1 2 3 →V3 2 *").unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::vector(vec![Value::Integer(2), Value::Integer(4), Value::Integer(6)]));
    }
}
