use crate::bytes::bytes::Bytes;

pub mod scoring {
    pub fn get_english_ascii_character_score(character: char) -> f64 {
        if !character.is_ascii_alphanumeric() {
            if character.is_ascii_punctuation() {
                return 0.0001
            } else if character.is_ascii_whitespace() {
                return 1.0
            }else if character.is_ascii_control() || character.is_ascii_graphic() {
                return 0.0
            }
            return 0.00000001;
        } else if character.is_ascii_alphabetic() {
            match character.to_ascii_lowercase() {
                'a' => 8.167,
                'b' => 1.492,
                'c' => 2.782,
                'd' => 4.253,
                'e' => 12.702,
                'f' => 2.228,
                'g' => 2.015,
                'h' => 6.094,
                'i' => 6.966,
                'j' => 0.253,
                'k' => 1.772,
                'l' => 4.025,
                'm' => 2.406,
                'n' => 6.749,
                'o' => 7.507,
                'p' => 1.929,
                'q' => 0.095,
                'r' => 5.987,
                's' => 6.327,
                't' => 9.056,
                'u' => 2.758,
                'w' => 2.360,
                'x' => 0.250,
                'y' => 1.974,
                'z' => 0.074,
                '\'' => 0.00001,
                _ => 0.0000000001
            }
        } else {
            return 0.000000000001;
        }
    }

    fn get_text_vec_magnitude(vector: &Vec<f64>) -> f64 {
        let squared_sum: f64 = vector.iter().fold(0.0,|sum: f64, value: &f64| sum + value.powf(2.0));
        squared_sum.sqrt()
    }

    fn get_text_vec_normalized(vector: &Vec<f64>, text_length: usize) -> Vec<f64> {
        match text_length as f64 {
            0.0 => vec![0.0; 27],
            length => vector.iter().map(|value| value / length).collect()
        }
    }

    pub fn euclidean_dist_to_english(text_character_frequencies: Vec<f64>, text_length: usize) -> f64 {
        let english_normalized_vector = vec![
            0.08167, // a
            0.01492, // b
            0.02782, // c
            0.04253, // d
            0.12702, // e
            0.02228, // f
            0.02015, // g
            0.06094, // h
            0.06966, // i
            0.00253, // j
            0.01772, // k
            0.04025, // l
            0.02406, // m
            0.06749, // n
            0.07507, // o
            0.01929, // p
            0.00095, // q
            0.05987, // r
            0.06327, // s
            0.09056, // t
            0.02758, // u
            0.00978, // v
            0.02360, // w
            0.00250, // x
            0.01974, // y
            0.00074  // z
        ];
        let normalized_text_vector = get_text_vec_normalized(&text_character_frequencies, text_length);
        let diff_vector = normalized_text_vector
            .into_iter()
            .zip(english_normalized_vector.into_iter())
            .map(|(a, b)| b - a)
            .collect();
        get_text_vec_magnitude(&diff_vector)
    }

    pub fn kl_divergence_to_english(text_character_frequencies: &Vec<f64>, text_length: usize) -> f64 {
        let english_normalized_vector = vec![
            0.0609, // a
            0.0105, // b
            0.0284, // c
            0.0292, // d
            0.1136, // e
            0.0179, // f
            0.0138, // g
            0.0341, // h
            0.0544, // i
            0.0024, // j
            0.0041, // k
            0.0292, // l
            0.0276, // m
            0.0544, // n
            0.0600, // o
            0.0195, // p
            0.0024, // q
            0.0495, // r
            0.0568, // s
            0.0803, // t
            0.0243, // u
            0.0097, // v
            0.0138, // w
            0.0024, // x
            0.0130, // y
            0.0003, // z
            12.170, // space
            6.5700  // other
        ];
        let normalized_text_vector = get_text_vec_normalized(text_character_frequencies, text_length);
        normalized_text_vector
            .into_iter()
            .zip(english_normalized_vector.into_iter())
            .map(|(p, q)| 
                if p == 0.0 { 99999.0 } 
                else { p * (p / q).log(2.0)}
            )
            .sum()
    }

    pub fn get_character_frequency_vec(message: &String) -> Vec<f64>{
        let mut character_frequencies: Vec<f64> = vec![0.0; 29];
        for character in message.chars() {
            match character {
                ('a'..='z') => {
                    let index = (character as u64) - 97;
                    character_frequencies[index as usize] += 1.0
                },
                ('A'..='Z') => {
                    let index = (character as u64) - 65;
                    character_frequencies[index as usize] += 1.0
                },
                ' ' => {
                    character_frequencies[27 as usize] += 1.0
                }
                _ => {
                    character_frequencies[28 as usize] += 1.0
                }
            }
        }
        character_frequencies
    }
}

pub enum StringOrBytes<'a>{
    Bytes(Bytes),
    String(String),
    ByteArray(&'a[u8])
}

pub fn hamming_distance(message_a: &StringOrBytes, message_b: &StringOrBytes) -> u32 {
    let a = match message_a {
        StringOrBytes::Bytes(message) => {
            message.to_ascii_string().unwrap().chars().map(|x| x as u8).collect()
        },
        StringOrBytes::String(message) => {
            message.chars().map(|x| x as u8).collect()
        },
        StringOrBytes::ByteArray(message) => {
            message.to_vec()
        }
    };

    let b = match message_b {
        StringOrBytes::Bytes(message) => {
            message.to_ascii_string().unwrap().chars().map(|x| x as u8).collect()
        },
        StringOrBytes::String(message) => {
            message.chars().map(|x| x as u8).collect()
        },
        StringOrBytes::ByteArray(message) => {
            message.to_vec()
        }
    };

    a.iter().zip(b).fold(0, |total, (a , b)| total + ((*a as u8) ^ (b as u8)).count_ones())
}

#[cfg(test)]
mod tests {
    use crate::{scoring::{hamming_distance, StringOrBytes}, bytes::bytes::Bytes};

    #[test]
    fn test_hamming_distance_string() {
        let test_string_a = StringOrBytes::String(String::from("this is a test"));
        let test_string_b = StringOrBytes::String(String::from("wokka wokka!!!"));
        let expected_hamming_distance = 37;
        assert_eq!(hamming_distance(&test_string_a, &test_string_b), expected_hamming_distance);

    }

    #[test]
    fn test_hamming_distance_bytes() {
        let test_string_a = StringOrBytes::Bytes(Bytes::from_ascii_string("this is a test"));
        let test_string_b = StringOrBytes::Bytes(Bytes::from_ascii_string("wokka wokka!!!"));
        let expected_hamming_distance = 37;
        assert_eq!(hamming_distance(&test_string_a, &test_string_b), expected_hamming_distance);

    }
}