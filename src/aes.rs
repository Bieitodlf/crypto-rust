pub mod aes {
    use std::fmt::{self, Formatter, Display};
    use std::thread::{self, JoinHandle};
    use std::fmt::Error;
    use pad::PadStr;

    const WORD_BYTE_COUNT: usize = 4;

    const K128_WORD_COUNT: usize = 4;
    const K128_ROUND_COUNT: usize = 10;
    const K128_HEX_KEY_LENGTH: usize = K128_WORD_COUNT * WORD_BYTE_COUNT * 2;
    const K192_WORD_COUNT: usize = 6;
    const K192_ROUND_COUNT: usize = 12;
    const K192_HEX_KEY_LENGTH: usize = K192_WORD_COUNT * WORD_BYTE_COUNT * 2;
    const K256_WORD_COUNT: usize = 8;
    const K256_ROUND_COUNT: usize = 14;
    const K256_HEX_KEY_LENGTH: usize = K256_WORD_COUNT * WORD_BYTE_COUNT * 2;

    const IRREDUCIBLE_POLY: u8 = 0x1b;
    // 0001 1011
    // const MIX_COLS_POLY: CypherWord = CypherWord {value: [0x02, 0x01, 0x01, 0x01]};
    // const MULTIPLY_INVERSE_POLY: CypherWord = CypherWord {value: [0x0b, 0x0d, 0x09, 0x0e]};
    // const MULTIPLY_MOD_POLY: CypherWord = CypherWord {value: [0x01, 0x00, 0x00, 0x01]};
    const ROUND_CONST_BASE: CypherWord = CypherWord {value: [1, 0, 0, 0]};

    const S_SUB: [[u8; 16]; 16] = [
        [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
        [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
        [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
        [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
        [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
        [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
        [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
        [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
        [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
        [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
        [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
        [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
        [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
        [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
        [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
        [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]
    ];

    const S_SUB_INVERSE: [[u8; 16]; 16] = [
        [0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb],
        [0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb],
        [0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e],
        [0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25],
        [0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92],
        [0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84],
        [0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06],
        [0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b],
        [0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73],
        [0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e],
        [0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b],
        [0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4],
        [0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f],
        [0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef],
        [0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61],
        [0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d]
    ];

    #[derive(Clone, Copy)]
    pub enum CypherKey {
        K128([CypherWord; K128_WORD_COUNT]),
        K192([CypherWord; K192_WORD_COUNT]),
        K256([CypherWord; K256_WORD_COUNT])
    }

    impl CypherKey {
        fn new_128() -> CypherKey {
            CypherKey::K128([CypherWord::new(); K128_WORD_COUNT])
        }

        fn new_192() -> CypherKey {
            CypherKey::K192([CypherWord::new(); K192_WORD_COUNT])
        }

        fn new_256() -> CypherKey {
            CypherKey::K256([CypherWord::new(); K256_WORD_COUNT])
        }

        fn from_list_k128(list: [[u8; WORD_BYTE_COUNT]; K128_WORD_COUNT]) -> CypherKey {
            let mut key = CypherKey::new_128();
            for i in 0..K128_WORD_COUNT {
                key[i] = CypherWord::from_array(list[i]);
            }
            key
        }

        fn from_list_k192(list: [[u8; WORD_BYTE_COUNT]; K192_WORD_COUNT]) -> CypherKey {
            let mut key = CypherKey::new_192();
            for i in 0..K192_WORD_COUNT {
                key[i] = CypherWord::from_array(list[i]);
            }
            key
        }

        fn from_list_k256(list: [[u8; WORD_BYTE_COUNT]; K256_WORD_COUNT]) -> CypherKey {
            let mut key = CypherKey::new_256();
            for i in 0..K256_WORD_COUNT {
                key[i] = CypherWord::from_array(list[i]);
            }
            key
        }

        fn from_string(key_string: &str) -> Result<CypherKey, Error> {
            match key_string.len() {
                K128_HEX_KEY_LENGTH => {
                    let key_chars: Vec<char> = key_string.chars().collect();
                    let mut key = CypherKey::new_128();
                    for i in 0..(K128_HEX_KEY_LENGTH / 2) {
                        let left_char = match key_chars[i * 2].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        let right_char = match key_chars[i * 2 + 1].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        key[i/4][i%4] = (left_char << 4) + right_char;
                    }
                    Ok(key)
                },
                K192_HEX_KEY_LENGTH => {
                    let key_chars: Vec<char> = key_string.chars().collect();
                    let mut key = CypherKey::new_192();
                    for i in 0..(K192_HEX_KEY_LENGTH / 2) {
                        let left_char = match key_chars[i * 2].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        let right_char = match key_chars[i * 2 + 1].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        key[i/4][i%4] = (left_char << 4) + right_char;
                    }
                    Ok(key)
                },
                K256_HEX_KEY_LENGTH => {
                    let key_chars: Vec<char> = key_string.chars().collect();
                    let mut key = CypherKey::new_256();
                    for i in 0..(K256_HEX_KEY_LENGTH / 2) {
                        let left_char = match key_chars[i * 2].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        let right_char = match key_chars[i * 2 + 1].to_digit(16) {
                            Some(char) => {char as u8},
                            None => {return Err(Error)}
                        };
                        key[i/4][i%4] = (left_char << 4) + right_char;
                    }
                    Ok(key)
                },
                _ => {
                    Err(Error)
                }
            }
        }
    }
    
    impl std::ops::Index<usize> for CypherKey {
        type Output = CypherWord;
        fn index(&self, i: usize) -> &Self::Output {
            match self {
                CypherKey::K128(value) => {&value[i]},
                CypherKey::K192(value) => {&value[i]},
                CypherKey::K256(value) => {&value[i]}
            }
        }
    }

    impl std::ops::IndexMut<usize> for CypherKey {
        fn index_mut(&mut self, i: usize) -> &mut Self::Output {
            match self {
                CypherKey::K128(value) => {&mut value[i]},
                CypherKey::K192(value) => {&mut value[i]},
                CypherKey::K256(value) => {&mut value[i]}
            }
        }
    }

    impl Display for CypherKey {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            match self {
                CypherKey::K128(_) => {
                    write!(f, "{}\n{}\n{}\n{}\n", self[0], self[1], self[2], self[3])
                },
                CypherKey::K192(_) => {
                    write!(f, "{}\n{}\n{}\n{}\n{}\n{}\n", self[0], self[1], self[2], self[3], self[4], self[5])
                },
                CypherKey::K256(_) => {
                    write!(f, "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n", self[0], self[1], self[2], self[3], self[4], self[5], self[6], self[7])
                }
            }
        }
    } 

    const K128_KEY_SCHEDULE_SIZE: usize = WORD_BYTE_COUNT * (K128_ROUND_COUNT + 1);
    const K192_KEY_SCHEDULE_SIZE: usize = WORD_BYTE_COUNT * (K192_ROUND_COUNT + 1);
    const K256_KEY_SCHEDULE_SIZE: usize = WORD_BYTE_COUNT * (K256_ROUND_COUNT + 1);

    #[derive(Debug, PartialEq, Eq)]
    enum KeySchedule {
        K128([CypherWord; K128_KEY_SCHEDULE_SIZE]),
        K192([CypherWord; K192_KEY_SCHEDULE_SIZE]),
        K256([CypherWord; K256_KEY_SCHEDULE_SIZE]),
    }

    impl <Idx> std::ops::Index<Idx> for KeySchedule
    where Idx: std::slice::SliceIndex<[CypherWord]> {
        type Output = Idx::Output;
        fn index(&self, i: Idx) -> &Self::Output {
            match self {
                KeySchedule::K128(value) => {&value[i]},
                KeySchedule::K192(value) => {&value[i]},
                KeySchedule::K256(value) => {&value[i]}
            }
        }
    }

    impl std::ops::IndexMut<usize> for KeySchedule {
        fn index_mut(&mut self, i: usize) -> &mut Self::Output {
            match self {
                KeySchedule::K128(value) => {&mut value[i]},
                KeySchedule::K192(value) => {&mut value[i]},
                KeySchedule::K256(value) => {&mut value[i]}
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct CypherWord {
        value: [u8; WORD_BYTE_COUNT]
    }
    impl CypherWord {
        fn new() -> CypherWord {
            CypherWord {
                value: [0; WORD_BYTE_COUNT]
            }
        }

        fn from_array(word_array: [u8; WORD_BYTE_COUNT] ) -> CypherWord {
            CypherWord {
                value: word_array
            }
        }

        fn from_u32(word_32: u32) -> CypherWord {
            CypherWord::from_array([
                ((word_32 >> 24) & 0x000000FF) as u8,
                ((word_32 >> 16) & 0x000000FF) as u8,
                ((word_32 >> 8) & 0x000000FF) as u8,
                (word_32 & 0x000000FF) as u8
            ])
        }
    }

    impl Display for CypherWord {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(
                f, "0x{:02X}{:02X}{:02X}{:02X}",
                self[0], self[1], self[2], self[3]
            )
        }
    }

    impl std::ops::Index<usize> for CypherWord {
        type Output = u8;
        fn index(&self, i: usize) -> &Self::Output {
            &self.value[i]
        }
    }

    impl std::ops::IndexMut<usize> for CypherWord {
        fn index_mut(&mut self, i: usize) -> &mut Self::Output {
            &mut self.value[i]
        }
    }

    impl std::ops::BitXor for CypherWord {
        type Output = Self;

        // rhs is the "right-hand side" of the expression `a ^ b`
        fn bitxor(self, rhs: Self) -> Self::Output {
            let mut result = Self::new();
            for i in 0..WORD_BYTE_COUNT {
                result[i] = self[i] ^ rhs[i];
            }
            result
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct CypherArray {
        value: [CypherWord; 4]
    }

    impl CypherArray {
        fn new() -> CypherArray {
            CypherArray {
                value: [CypherWord::new(); 4]
            }
        }

        fn from_string(cypher_string: &str) -> Result<CypherArray, Error> {
            let mut data = CypherArray::new();
            if cypher_string.len() > WORD_BYTE_COUNT * 8 {
                panic!("Cypher block must be 4 cypher words long. 4 * 4 Bytes, so 4 * 4 * 2 hex encoded bytes.")
            }
            let mut i: usize = 0;
            let mut cypher_iter = cypher_string.chars();
            while let (Some(char_1), Some(char_2)) = (cypher_iter.next(), cypher_iter.next()) {
                match (char_1.to_digit(16), char_2.to_digit(16)) {
                    (Some(char_1), Some(char_2)) => {
                        let value = ((char_1 as u8) << 4) + (char_2 as u8);
                        data[i/4][i%4] = value;
                    },
                    (Some(char_1), None) => {
                        let value = ((char_1 as u8) << 4);
                    },
                    (None, None) => {
                        let value: u8 = 0;
                    }
                    _ => {
                        return Err(Error);
                    }
                }
                i += 1;
            }
            Ok(data)
        }

        fn to_string(&self) -> Result<String, Error> {
            let mut cypher_string = String::from("");
            for word in self.value {
                cypher_string.push_str(&word.to_string());
            }
            Ok(cypher_string)
        }
    }

    impl std::ops::Index<usize> for CypherArray {
        type Output = CypherWord;
        fn index(&self, i: usize) -> &Self::Output {
            &self.value[i]
        }
    }

    impl std::ops::IndexMut<usize> for CypherArray {
        fn index_mut(&mut self, i: usize) -> &mut Self::Output {
            &mut self.value[i]
        }
    }

    impl Display for CypherArray {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{}\n{}\n{}\n{}\n", self[0], self[1], self[2], self[3])
        }
    } 

    pub struct Data<'a> {
        input: CypherArray,
        state: CypherArray,
        key: &'a CypherKey,
        key_schedule: KeySchedule,
        round: usize
    }

    fn xtime(a: u8) -> u8{
        if (a & 0b10000000) == 0 {
            a << 1 
        } else {
            (a << 1) ^ IRREDUCIBLE_POLY
        }
    }
    
    fn mult(mut a: u8, mut b: u8) -> u8 {
        /* We find the result by calculating xtime(a) for each order of the b polynomial
        with a non-zero coeficient and xoring it  */
        let mut result = 0;   
        while b != 0 {
            if b & 0x01 == 1 {
                result ^= a;
            }
            a = xtime(a);
            b >>= 1;
        }
        result
    }
    fn cypher_word_add(a: CypherWord, b: CypherWord) -> CypherWord {
        CypherWord::from_array([
            a[0] ^ b[0],
            a[1] ^ b[1],
            a[2] ^ b[2],
            a[3] ^ b[3],
        ])
    }
    fn cypher_column_mult(a: CypherWord, b: CypherWord) -> CypherWord {
        CypherWord::from_array([
            
                mult(a[0], b[0]) ^ mult(a[3], b[1])
                ^ mult(a[2], b[2]) ^ mult(a[1], b[3]),

                mult(a[1], b[0]) ^ mult(a[0], b[1])
                ^ mult(a[3], b[2]) ^ mult(a[2], b[3]),

                mult(a[2], b[0]) ^ mult(a[1], b[1]) 
                ^ mult(a[0], b[2]) ^ mult(a[3], b[3]),

                mult(a[3], b[0]) ^ mult(a[2], b[1])
                ^ mult(a[1], b[2]) ^ mult(a[0], b[3])
        ])
    }

    fn add_round_key(data: &Data, round: &usize) -> CypherArray {
        let mut working_state = CypherArray::new();
        let key_schedule_round_start = round * WORD_BYTE_COUNT;
        for i in 0..4 {
            working_state[i] = data.state[i] ^ data.key_schedule[key_schedule_round_start + i];
        }
        working_state
    }
    
    fn inv_add_round_key(data: &Data, round: &usize) -> CypherArray {
        let mut working_state = CypherArray::new();
        let key_schedule_round_start = round * WORD_BYTE_COUNT;
        for i in 0..4 {
            working_state[i] = data.state[i] ^ data.key_schedule[key_schedule_round_start + i];
        }
        working_state
    }

    fn sub_bytes(state: &mut CypherArray) {
        for n in 0..4 {
            for m in 0..4 {
                state[n][m] = S_SUB[(state[n][m] >> 4) as usize][(state[n][m] & 0x0f) as usize];
            }
        }
    }
    
    fn inv_sub_bytes(state: &mut CypherArray) {
        for n in 0..4 {
            for m in 0..4 {
                state[n][m] = S_SUB_INVERSE[(state[n][m] >> 4) as usize][(state[n][m] & 0x0f) as usize];
            }
        }
    }

    fn shift_rows(state: &CypherArray) -> CypherArray {
        let mut working_state = CypherArray::new();
        working_state[0] = CypherWord::from_array([state[0][0], state[1][1], state[2][2], state[3][3]]);
        working_state[1] = CypherWord::from_array([state[1][0], state[2][1], state[3][2], state[0][3]]);
        working_state[2] = CypherWord::from_array([state[2][0], state[3][1], state[0][2], state[1][3]]);
        working_state[3] = CypherWord::from_array([state[3][0], state[0][1], state[1][2], state[2][3]]);
        working_state
    }

    fn inv_shift_rows(state: &CypherArray) -> CypherArray {
        let mut working_state = CypherArray::new();
        working_state[0] = CypherWord::from_array([state[0][0], state[3][1], state[2][2], state[1][3]]);
        working_state[1] = CypherWord::from_array([state[1][0], state[0][1], state[3][2], state[2][3]]);
        working_state[2] = CypherWord::from_array([state[2][0], state[1][1], state[0][2], state[3][3]]);
        working_state[3] = CypherWord::from_array([state[3][0], state[2][1], state[1][2], state[0][3]]);
        working_state
    }

    fn mix_columns(state: &CypherArray) -> CypherArray {
        let mut working_state = CypherArray::new();
        for n in 0..4 {
            working_state[n][0] = mult(state[n][0], 0x02) ^ mult(state[n][1], 0x03) ^ state[n][2] ^ state[n][3];
            working_state[n][1] = state[n][0] ^ mult(state[n][1], 0x02) ^ mult(state[n][2], 0x03) ^ state[n][3];
            working_state[n][2] = state[n][0] ^ state[n][1] ^ mult(state[n][2], 0x02) ^ mult(state[n][3], 0x03);
            working_state[n][3] = mult(state[n][0], 0x03) ^ state[n][1] ^ state[n][2] ^ mult(state[n][3], 0x02);
        }
        working_state  
    }

    fn inv_mix_columns(state: &CypherArray) -> CypherArray {
        let mut working_state = CypherArray::new();
        for n in 0..4 {
            working_state[n][0] = mult(state[n][0], 0x0e) ^ mult(state[n][1], 0x0b) ^ mult(state[n][2], 0x0d) ^ mult(state[n][3], 0x09);
            working_state[n][1] = mult(state[n][0], 0x09) ^ mult(state[n][1], 0x0e) ^ mult(state[n][2], 0x0b) ^ mult(state[n][3], 0x0d);
            working_state[n][2] = mult(state[n][0], 0x0d) ^ mult(state[n][1], 0x09) ^ mult(state[n][2], 0x0e) ^ mult(state[n][3], 0x0b);
            working_state[n][3] = mult(state[n][0], 0x0b) ^ mult(state[n][1], 0x0d) ^ mult(state[n][2], 0x09) ^ mult(state[n][3], 0x0e);
        }
        working_state  
    }

    fn rotate_word(word: &CypherWord) -> CypherWord {
        let mut rotated_word: CypherWord = CypherWord::new();
        for i in 0..(WORD_BYTE_COUNT - 1) {
            rotated_word[i] = word[i + 1];
        }
        rotated_word[WORD_BYTE_COUNT - 1] = word[0];
        rotated_word
    }

    fn sub_word(word: &CypherWord) -> CypherWord {
        let mut sub_word = word.clone();
        for i in 0..4 {
            sub_word[i] = S_SUB[(sub_word[i] >> 4) as usize][(sub_word[i] & 0x0f) as usize];
        }
        sub_word
    }
    
    fn key_expansion(key: &CypherKey) -> (usize, KeySchedule) {
        let (mut key_schedule, key_word_count, rounds) = match key {
            CypherKey::K128(_) => {
                (
                    KeySchedule::K128([CypherWord::new(); K128_KEY_SCHEDULE_SIZE]),
                    K128_WORD_COUNT,
                    K128_ROUND_COUNT
                )
            },
            CypherKey::K192(_) => {
                (
                    KeySchedule::K192([CypherWord::new(); K192_KEY_SCHEDULE_SIZE]),
                    K192_WORD_COUNT,
                    K192_ROUND_COUNT
                )
            },
            CypherKey::K256(_) => {
                (
                    KeySchedule::K256([CypherWord::new(); K256_KEY_SCHEDULE_SIZE]), 
                    K256_WORD_COUNT,
                    K256_ROUND_COUNT
                )
            }
        };
        for i in 0..key_word_count {
            key_schedule[i] = key[i];
        }

        let mut round_const: CypherWord = ROUND_CONST_BASE.clone();
        for i in key_word_count..(WORD_BYTE_COUNT * (rounds + 1)) {
            let mut temp = key_schedule[i - 1].clone();
            if i % key_word_count == 0 {
                // println!("rotate_word: {}", rotate_word(&temp));
                // println!("sub_word: {}", sub_word(&rotate_word(&temp)));
                // println!("round_const: {}", round_const);
                temp = sub_word(&rotate_word(&temp)) ^ round_const;
                // println!("after XOR: {}", temp);
                round_const[0] = round_const[0] << 1;
                if round_const[0] == 0 {
                    round_const[0] = 0x1b;
                }
            } else if (key_word_count > 6) && (i % key_word_count == 4) {
                temp = sub_word(&temp);
            }
            key_schedule[i] = key_schedule[i - key_word_count] ^ temp;
            // println!("w[i-nk]: {}", key_schedule[i - key_word_count]);
            // println!("W[i]: {}", key_schedule[i]);
        }
        (rounds, key_schedule)
    }

    pub fn cypher(key: &CypherKey, input: CypherArray) -> CypherArray {
        let (rounds, key_schedule) = key_expansion(&key);
        let mut data = Data {
            input: input,
            state: input.clone(),
            key: &key,
            key_schedule: key_schedule,
            round: 0
        };
        data.state = add_round_key(&data, &0);
        for round in 1..(rounds) {
            // println!("Add_Round_Key:\n{}", data.state);
            sub_bytes(&mut data.state);
            // println!("sub_bytes:\n{}", data.state);
            data.state = shift_rows(&data.state);
            // println!("shift_rows:\n{}", data.state);
            data.state = mix_columns(&data.state);
            // println!("mix_columns:\n{}", data.state);
            data.state = add_round_key(&data, &round);
        }
        sub_bytes(&mut data.state);
        data.state = shift_rows(&data.state);
        data.state = add_round_key(&data, &rounds);
        data.state
    }

    pub fn inv_cypher(key: &CypherKey, input: CypherArray) -> CypherArray {
        let (rounds, key_schedule) = key_expansion(&key);
        // println!("{}", key_schedule);
        let mut data = Data {
            input: input,
            state: input.clone(),
            key: &key,
            key_schedule: key_schedule,
            round: rounds
        };
        data.state = inv_add_round_key(&data, &rounds);
        println!("inv_add_round_key:\n{}", data.state);
        for round in (1..(rounds)).rev() {
            data.state = inv_shift_rows(&data.state);
            println!("inv_shift_rows:\n{}", data.state);
            inv_sub_bytes(&mut data.state);
            println!("inv_sub_bytes:\n{}", data.state);
            data.state = inv_add_round_key(&data, &round);
            println!("Inv_Add_Round_Key:\n{}", data.state);
            data.state = inv_mix_columns(&data.state);
            println!("inv_mix_columns:\n{}", data.state);
            
        }
        data.state = inv_shift_rows(&data.state);
        inv_sub_bytes(&mut data.state);
        data.state = inv_add_round_key(&data, &0);
        data.state
    }

    pub fn ecb(mut plaintext: String, key_string: &str) -> Result<String, Error> {
        let key = match CypherKey::from_string(key_string) {
            Ok(cypher_key) => {cypher_key},
            Err(e) => return Err(e)
        };
        let padding_length = plaintext.len() / (WORD_BYTE_COUNT * 4);
        if padding_length != 0 {
            plaintext.push(char::from_u32(1).unwrap());
            plaintext.pad_to_width(plaintext.len() + padding_length);
        }
        let block_iter = plaintext.char_indices()
                                            .flat_map(|(from, _)| {
                                                plaintext[from ..].char_indices()
                                                .skip((WORD_BYTE_COUNT * 4 * 2) - 1)
                                                .next()
                                                .map(|(to, c)| {
                                                    &plaintext[from .. from + to + c.len_utf8()]
                                                })
                                            });
        let block_thread_join_handles: Vec<JoinHandle<String>> = block_iter.map(|block| {
            let plain_block = match CypherArray::from_string(block) {
                Ok(plain_block) => plain_block,
                Err(_) => {panic!("Error occurred formatting input text.")}
            };
            thread::spawn(move || {
                let cypher_block = cypher(&key, plain_block);
                match cypher_block.to_string() {
                    Ok(cypher_block) => cypher_block,
                    Err(_) => {panic!("could not turn cypher text into valid string");}
                }
            })
        }).collect();
        let mut cypher_text = String::from("");
        for cypher_block in block_thread_join_handles.into_iter() {
            match cypher_block.join() {
                Ok(cypher_word) => {
                    cypher_text.push_str(&cypher_word);
                },
                Err(_) => {panic!("could not turn cypher into valid string.")}
            }
        }
        Ok(cypher_text)
    }


    #[cfg(test)]
    mod aestests {
        use super::*;
        static TEST_128_KEY_SCHEDULE: [u32; 44] = [
            0x2b7e1516,
            0x28aed2a6,
            0xabf71588,
            0x09cf4f3c,
            0xa0fafe17,
            0x88542cb1,
            0x23a33939,
            0x2a6c7605,
            0xf2c295f2,
            0x7a96b943,
            0x5935807a,
            0x7359f67f,
            0x3d80477d,
            0x4716fe3e,
            0x1e237e44,
            0x6d7a883b,
            0xef44a541,
            0xa8525b7f,
            0xb671253b,
            0xdb0bad00,
            0xd4d1c6f8,
            0x7c839d87,
            0xcaf2b8bc,
            0x11f915bc,
            0x6d88a37a,
            0x110b3efd,
            0xdbf98641,
            0xca0093fd,
            0x4e54f70e,
            0x5f5fc9f3,
            0x84a64fb2,
            0x4ea6dc4f,
            0xead27321,
            0xb58dbad2,
            0x312bf560,
            0x7f8d292f,
            0xac7766f3,
            0x19fadc21,
            0x28d12941,
            0x575c006e,
            0xd014f9a8,
            0xc9ee2589,
            0xe13f0cc8,
            0xb6630ca6
        ];

        #[test]
        fn test_xtime() {
            assert_eq!(xtime(0x57), 0xae);
            assert_eq!(xtime(0xae), 0x47);
        }

        #[test]
        fn test_mult() {
            let a = 0x57;
            let b = 0x13;
            assert_eq!(mult(a, b), 0xfe);
        }

        #[test]
        fn test_cypher_word_add() {
            let word_a: CypherWord = CypherWord::from_array([0b10101010, 0b10101010, 0b10101010, 0b10101010]);
            let word_b: CypherWord = CypherWord::from_array([0b10101010, 0b01010101, 0b11111111, 0b00000000]);
            let expected: CypherWord = CypherWord::from_array([0b00000000, 0b11111111, 0b01010101, 0b10101010]);
            assert_eq!(cypher_word_add(word_a, word_b), expected);
        }

        #[test]
        fn test_sub_bytes() {}

        #[test]
        fn test_shift_rows() {
            let input: CypherArray = CypherArray { 
                value: [
                    CypherWord{value: [0xd4, 0x27, 0x11, 0xae]},
                    CypherWord{value: [0xe0, 0xbf, 0x98, 0xf1]},
                    CypherWord{value: [0xb8, 0xb4, 0x5d, 0xe5]},
                    CypherWord{value: [0x1e, 0x41, 0x52, 0x30]}
                ]
            };
            let expected_output: CypherArray = CypherArray { 
                value: [
                    CypherWord{value: [0xd4, 0xbf, 0x5d, 0x30]},
                    CypherWord{value: [0xe0, 0xb4, 0x52, 0xae]},
                    CypherWord{value: [0xb8, 0x41, 0x11, 0xf1]},
                    CypherWord{value: [0x1e, 0x27, 0x98, 0xe5]}
                ]
            };
            let result = shift_rows(&input);
            assert_eq!(result, expected_output);
        }

        #[test]
        fn test_inv_shift_rows() {
            let input: CypherArray = CypherArray::from_string("7ad5fda789ef4e272bca100b3d9ff59f").unwrap();
            let expected_output: CypherArray = CypherArray::from_string("7a9f102789d5f50b2beffd9f3dca4ea7").unwrap();
            let result = inv_shift_rows(&input);
            assert_eq!(result, expected_output);
        }

        #[test]
        fn test_mix_columns() {
            let input: CypherArray = CypherArray { 
                value: [
                    CypherWord{value: [0xd4, 0xbf, 0x5d, 0x30]},
                    CypherWord{value: [0xe0, 0xb4, 0x52, 0xae]},
                    CypherWord{value: [0xb8, 0x41, 0x11, 0xf1]},
                    CypherWord{value: [0x1e, 0x27, 0x98, 0xe5]}
                ]
            };
            let expected_output: CypherArray = CypherArray { 
                value: [
                    CypherWord{value: [0x04, 0x66, 0x81, 0xe5]},
                    CypherWord{value: [0xe0, 0xcb, 0x19, 0x9a]},
                    CypherWord{value: [0x48, 0xf8, 0xd3, 0x7a]},
                    CypherWord{value: [0x28, 0x06, 0x26, 0x4c]}
                ]
            };
            let output = mix_columns(&input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_inv_mix_columns() {
            let input: CypherArray = CypherArray::from_string("e9f74eec023020f61bf2ccf2353c21c7").unwrap();
            let expected_output: CypherArray = CypherArray::from_string("54d990a16ba09ab596bbf40ea111702f").unwrap();
            let output = inv_mix_columns(&input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_split_key() {}

        #[test]
        fn test_rotate_word() {
            let test_value: CypherWord = CypherWord::from_array([0x09, 0xcf, 0x4f, 0x3c]);
            let expected_value: CypherWord = CypherWord::from_array([0xcf, 0x4f, 0x3c, 0x09]);
            assert_eq!(rotate_word(&test_value), expected_value)
        }

        #[test]
        fn test_sub_word() {
            let test_value: CypherWord = CypherWord::from_array([0xcf, 0x4f, 0x3c, 0x09]);
            let expected_value: CypherWord = CypherWord::from_array([0x8a, 0x84, 0xeb, 0x01]);
            assert_eq!(sub_word(&test_value), expected_value);
        }

        #[test]
        fn test_key_expansion() {
            let mut key_schedule: KeySchedule = KeySchedule::K128([CypherWord::new(); K128_KEY_SCHEDULE_SIZE]);
            for i in 0..TEST_128_KEY_SCHEDULE.len() {
                key_schedule[i] = CypherWord::from_u32(TEST_128_KEY_SCHEDULE[i]);
            }
            let test_value: CypherKey  = CypherKey::K128([
                CypherWord::from_u32(0x2b7e1516),
                CypherWord::from_u32(0x28aed2a6),
                CypherWord::from_u32(0xabf71588),
                CypherWord::from_u32(0x09cf4f3c)
            ]);
            let (_, expanded_key) = key_expansion(&test_value);
            assert_eq!(expanded_key, key_schedule);
        }

        #[test]
        fn test_cypher() {
            let input: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            // println!("{}", input);
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("69c4e0d86a7b0430d8cdb78070b4c55a").unwrap();
            // println!("{}", expected_output);
            let output = cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_inv_cypher() {
            let input: CypherArray = CypherArray::from_string("69c4e0d86a7b0430d8cdb78070b4c55a").unwrap();
            // println!("{}", input);
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            // println!("{}", expected_output);
            let output = inv_cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_cypher_192() {
            let input: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f1011121314151617").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("dda97ca4864cdfe06eaf70a0ec0d7191").unwrap();
            let output = cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_inv_cypher_192() {
            let input: CypherArray = CypherArray::from_string("dda97ca4864cdfe06eaf70a0ec0d7191").unwrap();
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f1011121314151617").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            let output = inv_cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_cypher_256() {
            let input: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("8ea2b7ca516745bfeafc49904b496089").unwrap();
            let output = cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_inv_cypher_256() {
            let input: CypherArray = CypherArray::from_string("8ea2b7ca516745bfeafc49904b496089").unwrap();
            let key = CypherKey::from_string("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f").unwrap();
            println!("{}", key);
            let expected_output: CypherArray = CypherArray::from_string("00112233445566778899aabbccddeeff").unwrap();
            let output = inv_cypher(&key, input);
            assert_eq!(output, expected_output);
        }

        #[test]
        fn test_ecb_128() {
            let plaintext_raw = String::from("6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710");
            // let mut plaintext_chars = plaintext_raw.chars().into_iter();
            // let mut plaintext = String::new();
            // while let (Some(char_1), Some(char_2)) = (plaintext_chars.by_ref().next(), plaintext_chars.by_ref().next()) {
            //     match (char_1.to_digit(16), char_2.to_digit(16)) {
            //         (Some(char_1), Some(char_2)) => {
            //             let value = ((char_1 as u8) << 4) + (char_2 as u8);
            //             plaintext.push(char::from_u32(value.into()).unwrap());
            //         },
            //         _ => {}
            //     }
            // }
            let key_text = "2b7e151628aed2a6abf7158809cf4f3c";
            let expected_cyphertext = "3ad77bb40d7a3660a89ecaf32466ef97f5d3d58503b9699de785895a96fdbaaf43b1cd7f598ece23881b00e3ed0306887b0c785e27e8ad3f8223207104725dd4";
            assert_eq!(expected_cyphertext, ecb(plaintext_raw, key_text).unwrap());
        }
    }
}
