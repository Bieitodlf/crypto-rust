pub mod bytes {
    use std::u8;
    // use std::str::FromStr;

    #[derive(Clone)]
    pub struct Bytes {
        pub data: Vec<u8>,
        b64_str: String,
        hex_str: String
    }
    impl Bytes {
        fn len(self: &Bytes) -> usize{
            self.data.len()
        }

        pub fn iter(self: &Bytes) -> std::slice::Iter<'_, u8> {
            self.data.iter()
        }

        pub fn new() -> Bytes {
            Bytes {
                data: Vec::new(),
                b64_str: String::new(),
                hex_str: String::new()
            }
        }

        pub fn from_vec(data: Vec<u8>) -> Bytes {
            Bytes {
                data,
                b64_str: String::new(),
                hex_str: String::new()
            }
        }

        fn b64_lookup(code: u8) -> char {
            match code {
                0 => 'A',
                1 => 'B',
                2 => 'C',
                3 => 'D',
                4 => 'E',
                5 => 'F',
                6 => 'G',
                7 => 'H',
                8 => 'I',
                9 => 'J',
                10 => 'K',
                11 => 'L',
                12 => 'M',
                13 => 'N',
                14 => 'O',
                15 => 'P',
                16 => 'Q',
                17 => 'R',
                18 => 'S',
                19 => 'T',
                20 => 'U',
                21 => 'V',
                22 => 'W',
                23 => 'X',
                24 => 'Y',
                25 => 'Z',
                26 => 'a',
                27 => 'b',
                28 => 'c',
                29 => 'd',
                30 => 'e',
                31 => 'f',
                32 => 'g',
                33 => 'h',
                34 => 'i',
                35 => 'j',
                36 => 'k',
                37 => 'l',
                38 => 'm',
                39 => 'n',
                40 => 'o',
                41 => 'p',
                42 => 'q',
                43 => 'r',
                44 => 's',
                45 => 't',
                46 => 'u',
                47 => 'v',
                48 => 'w',
                49 => 'x',
                50 => 'y',
                51 => 'z',
                52 => '0',
                53 => '1',
                54 => '2',
                55 => '3',
                56 => '4',
                57 => '5',
                58 => '6',
                59 => '7',
                60 => '8',
                61 => '9',
                62 => '+',
                63 => '/',
                _ => {
                    println!("char: {code}");
                    panic!("Invalid b64 value");
                }
            }
        }

        fn b64_reverse_lookup(input_char: char) -> Option<u8> {
            match input_char {
                ('A'..='Z') => {
                    Some(input_char as u8 - 65)
                },
                ('a'..='z') => {
                    Some((input_char as u8 - 97) + 26)
                },
                ('0'..='9') => {
                    Some((input_char as u8 - 48) + 52)
                },
                '+' => {
                    Some(62 as u8)
                },
                '/' => {
                    Some(63 as u8)
                },
                _ => None
            }
        }

        pub fn from_hex_str(source_string: &str) -> Bytes {
            let mut data: Vec<u8> = vec![];
            if source_string.len() % 2 > 0 {
                panic!("String is not valid hex")
            }
            let mut source_chars = source_string.chars();
            while let (Some(char_1), Some(char_2)) = (source_chars.by_ref().next(), source_chars.by_ref().next()) {
                match (char_1.to_digit(16), char_2.to_digit(16)) {
                    (Some(char_1), Some(char_2)) => {
                        let value = ((char_1 as u8) << 4) + (char_2 as u8);
                        data.push(value);
                    },
                    _ => {}
                }
            }
            Bytes::from_vec(data)
        }

        pub fn from_b64_str(source_string: &str) -> Bytes {
            let mut data: Vec<u8> = vec![];
            if source_string.len() % 4 != 0 {
                panic!("Input string is not of the correct length for valid b64.")
            }
            let mut source_chars = source_string.chars();
            while let (Some(digit_1), Some(digit_2), Some(digit_3), Some(digit_4)) = (source_chars.by_ref().next(), source_chars.by_ref().next(), source_chars.by_ref().next(), source_chars.by_ref().next()) {
                match (Bytes::b64_reverse_lookup(digit_1), Bytes::b64_reverse_lookup(digit_2), Bytes::b64_reverse_lookup(digit_3), Bytes::b64_reverse_lookup(digit_4)) {
                    (Some(digit_1), Some(digit_2), Some(digit_3), Some(digit_4)) => {
                        data.push(((digit_1 << 2) + (digit_2 >> 4)) as u8);
                        data.push(((digit_2 << 4) + (digit_3 >> 2)) as u8);
                        data.push(((digit_3 << 6) + digit_4) as u8);
                    },
                    (Some(digit_1), Some(digit_2), Some(digit_3), None) => {
                        if digit_4 == '=' {
                        data.push(((digit_1 << 2) + (digit_2 >> 4)) as u8);
                        data.push(((digit_2 << 4) + (digit_3 >> 2)) as u8);
                        data.push((digit_3 << 4) as u8);
                        } else {
                            panic!("Invalid character conversion encountered when trying to parse b64 string. Offending character {digit_4}.")
                        }
                    },
                    (Some(digit_1), Some(digit_2), None, None) => {
                        if digit_3 == '=' && digit_4 == '=' {
                        data.push(((digit_1 << 2) + (digit_2 >> 4)) as u8);
                        data.push((digit_2 << 4) as u8);
                        } else {
                            panic!("Invalid character conversion encountered when trying to parse b64 string. Offending character in {digit_3}, {digit_4}.")
                        }
                    },
                    _ => {
                        panic!("Invalid character conversion encountered when tyring to parse b64 string. Offending character in {digit_1}, {digit_2}, {digit_3}.")
                    }
                }
            }
            Bytes::from_vec(data)
        }

        pub fn to_hex_str(mut self) -> String{
            let mut hex_str = String::new();
            for char in self.data.into_iter() {
                hex_str.push(char::from_digit((char >> 4) as u32, 16).unwrap());
                hex_str.push(char::from_digit((char & 0x0F) as u32, 16).unwrap());
            }
            self.hex_str = hex_str;
            self.hex_str
        }

        pub fn to_b64_str(mut self) -> String {
            let mut b64_string: String = String::new();
            let mut byte_iter = self.data.into_iter();
            loop {
                
                match (byte_iter.next(), byte_iter.next(), byte_iter.next()) {
                    (Some(first), Some(second), Some(third)) => {
                        b64_string.push(Bytes::b64_lookup((first >> 2) & 0x3f));
                        b64_string.push(Bytes::b64_lookup(((first << 4) + (second >> 4)) & 0x3f));
                        b64_string.push(Bytes::b64_lookup(((second << 2) + (third >> 6)) & 0x3f));
                        b64_string.push(Bytes::b64_lookup(third & 0x3f))
                    },
                    (Some(first), Some(second), None) => {
                        b64_string.push(Bytes::b64_lookup(first >> 2));
                        b64_string.push(Bytes::b64_lookup((first << 4) + (second >> 4)));
                        b64_string.push(Bytes::b64_lookup(second << 2));
                        b64_string.push('=');
                    },
                    (Some(first), None, _) => {
                        b64_string.push(Bytes::b64_lookup(first >> 2));
                        b64_string.push(Bytes::b64_lookup(first << 2));
                        b64_string.push('=');
                        b64_string.push('=');
                    },
                    (_, _, _) => {
                        break;
                    }
                }
            }
            self.b64_str = b64_string;
            self.b64_str
        }

        pub fn from_ascii_string(source_string: &str) -> Bytes {
            let data: Vec<u8> = Vec::from(source_string.as_bytes());
            Bytes::from_vec(data)
        }

        pub fn to_ascii_string(&self) -> Result<String, &'static str> {
            let test_string_message = String::from_iter(
                self.data.iter()
                .map(|&x| char::from_u32(x as u32)
                    .unwrap()
                )
            );
            let mut ascii_text = String::new();
            for &octet in self.iter() {
                if (octet <= 32) || octet == 10 || octet == 13 {
                    match char::from_u32(octet as u32) {
                        Some(next_char) => ascii_text.push(next_char),
                        None => return Err("Invalid ascii character produced.")
                    }
                } else {
                    break
                }
            }
            Ok(test_string_message)
        }

        fn xor_byte_vec(bytes_a: &Bytes, bytes_b: &Bytes) -> Bytes {
            if bytes_a.len() == bytes_b.len() {
                let mut bytes_result = Bytes::new();
                for (byte_a, byte_b) in bytes_a.data.iter().zip(bytes_b.data.iter()) {
                    bytes_result.data.push(byte_a ^ byte_b)
                }
                bytes_result
            } else {panic!("Mismatched operand sizes not allowed for bitwise operations.")}
        }

        pub fn single_byte_xor(&mut self, key: &u8) {
            let mut result = Vec::new();
            for byte in self.iter() {
                result.push(key ^ byte)
            }
            self.data = result;
        }

        pub fn repeat_key_xor(&mut self, key: &[u8]) {
            let mut result = Vec::new();
            for (message_byte, key_byte) in self.iter().zip(key.into_iter().cycle()) {
                result.push(key_byte ^ message_byte);
            }
            self.data = result;
        }
    }

    impl<Idx> std::ops::Index<Idx> for Bytes
    where Idx: std::slice::SliceIndex<[u8]>,
    {
        type Output = Idx::Output;

        fn index(&self, index: Idx) -> &Self::Output {
            &self.data[index]
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn test_parse_hex_string() {
            let test_hex_string = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
            let expected_result = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
            let result = Bytes::from_hex_str(test_hex_string).to_b64_str();
            assert_eq!(expected_result, result)
        }

        #[test]
        fn test_parse_b64_string() {
            let test_b64_string = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
            let expected_result = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";
            let result = Bytes::from_b64_str(test_b64_string).to_hex_str();
            assert_eq!(expected_result, result)
        }

        #[test]
        fn test_xor_bytes() {
            let test_input_a = Bytes::from_vec(vec![0b10101010, 0b10101010]);
            let test_input_b = Bytes::from_vec(vec![0b11110000, 0b11110000]);
            let expected_result = Bytes::from_vec(vec![0b01011010, 0b01011010]);
            assert_eq!(expected_result.data, Bytes::xor_byte_vec(&test_input_a, &test_input_b).data)

        }

        #[test]
        fn test_fixed_xor_string() {
            let test_input = Bytes::from_hex_str("1c0111001f010100061a024b53535009181c");
            let test_xor = Bytes::from_hex_str("686974207468652062756c6c277320657965");
            let test_expected = Bytes::from_hex_str("746865206b696420646f6e277420706c6179");
            assert_eq!(Bytes::xor_byte_vec(&test_input, &test_xor).data, test_expected.data)
        }

        #[test]
        fn test_single_byte_xor() {
            let mut bytes_in = Bytes::from_hex_str("FFFFFFFF");
            let key = 0b10101010;
            let expected_result = "55555555";
            bytes_in.single_byte_xor(&key);
            assert_eq!(bytes_in.to_hex_str(), expected_result)
        }

        #[test]
        fn test_repeat_key_xor() {
            let test_message_lines = vec![
                "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"
                ];
            let test_cypher_lines = vec![
                "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
                ];
            let test_key = b"ICE";
            for (message_line, cypher_line) in test_message_lines.iter().zip(test_cypher_lines.iter()) {
                let mut bytes_in = Bytes::from_ascii_string(message_line);
                bytes_in.repeat_key_xor(test_key);
                assert_eq!(*cypher_line, bytes_in.to_hex_str());
            }
        }
    }
}