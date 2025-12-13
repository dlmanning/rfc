//! LSP transport layer for stdio communication.
//!
//! Handles reading and writing LSP messages over stdin/stdout
//! with proper Content-Length headers.

use std::io::{self, BufRead, BufReader, Read, Write};

/// Read an LSP message from the given reader.
///
/// LSP messages are prefixed with headers like:
/// ```text
/// Content-Length: 123\r\n
/// \r\n
/// {"jsonrpc":"2.0",...}
/// ```
pub fn read_message<R: Read>(reader: &mut BufReader<R>) -> io::Result<Option<String>> {
    // Read headers
    let mut content_length: Option<usize> = None;

    loop {
        let mut header = String::new();
        let bytes_read = reader.read_line(&mut header)?;

        if bytes_read == 0 {
            // EOF
            return Ok(None);
        }

        let header = header.trim();
        if header.is_empty() {
            // End of headers
            break;
        }

        if let Some(value) = header.strip_prefix("Content-Length: ") {
            content_length = Some(value.parse().map_err(|e| {
                io::Error::new(io::ErrorKind::InvalidData, format!("Invalid Content-Length: {}", e))
            })?);
        }
        // Ignore other headers (like Content-Type)
    }

    let content_length = content_length.ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidData, "Missing Content-Length header")
    })?;

    // Read the JSON content
    let mut content = vec![0u8; content_length];
    reader.read_exact(&mut content)?;

    String::from_utf8(content)
        .map(Some)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Invalid UTF-8: {}", e)))
}

/// Write an LSP message to the given writer.
pub fn write_message<W: Write>(writer: &mut W, message: &str) -> io::Result<()> {
    let content = message.as_bytes();
    write!(writer, "Content-Length: {}\r\n\r\n", content.len())?;
    writer.write_all(content)?;
    writer.flush()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn read_simple_message() {
        let input = "Content-Length: 13\r\n\r\n{\"test\":true}";
        let mut reader = BufReader::new(Cursor::new(input));
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, "{\"test\":true}");
    }

    #[test]
    fn read_with_extra_headers() {
        let input = "Content-Length: 13\r\nContent-Type: application/json\r\n\r\n{\"test\":true}";
        let mut reader = BufReader::new(Cursor::new(input));
        let msg = read_message(&mut reader).unwrap().unwrap();
        assert_eq!(msg, "{\"test\":true}");
    }

    #[test]
    fn read_eof_returns_none() {
        let input = "";
        let mut reader = BufReader::new(Cursor::new(input));
        let msg = read_message(&mut reader).unwrap();
        assert!(msg.is_none());
    }

    #[test]
    fn write_message_format() {
        let mut output = Vec::new();
        write_message(&mut output, "{\"test\":true}").unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "Content-Length: 13\r\n\r\n{\"test\":true}"
        );
    }
}
