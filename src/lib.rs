use std::fs::File;
use std::io::{self, BufRead};
use std::ops::Deref;
use std::path::Path;
use scoring::scoring::{kl_divergence_to_english, get_character_frequency_vec};

use crate::bytes::bytes::Bytes;
mod bytes;
mod scoring;
mod aes;

pub fn crack_single_byte_xor(message: &Bytes) -> (char, f64, Bytes) {
    let mut key_scores: Vec<f64> = Vec::new();
    let mut decoded_messages: Vec<Bytes> = Vec::new();
    for key in 0x20..=0x7E as u8 {
        let mut message_result = message.clone();
        message_result.single_byte_xor(&key);
        let decoded_message = 
            match  message_result.to_ascii_string() {
                    Ok(decoded_message_string) => decoded_message_string,
                    Err(_) => String::new()
            };
        let text_vector = get_character_frequency_vec(&decoded_message);
        let score = kl_divergence_to_english(&text_vector, decoded_message.len());
        key_scores.push(score);
        decoded_messages.push(message_result);
    }

    let best_key_index = key_scores
        .iter()
        .enumerate()
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
        .map(|(index, _)| index)
        .unwrap();
    (
        char::from_u32(best_key_index as u32 + 32).unwrap(),
        key_scores.remove(best_key_index),
        decoded_messages.remove(best_key_index)
    )
}

pub fn find_single_char_xor_in_file<P>(filename: P) -> (char, f64, Bytes)
where P: AsRef<Path> {
    if let Ok(lines) = read_file_lines(filename) {
        let mut decoded_lines: Vec<(char, f64, Bytes)> = Vec::new();
        for line in lines {
            if let Ok(line) = line {
                decoded_lines.push(
                    crack_single_byte_xor(
                        &Bytes::from_hex_str(&line)));
            }
        }

        let highest_scoring_index = decoded_lines
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.1.partial_cmp(&b.1).unwrap())
            .map(|(index, _)| index)
            .unwrap();

        return decoded_lines.remove(highest_scoring_index);
    }
    panic!("Could not read file.")
    
}

fn get_repeat_xor_likely_key_size(message: &Bytes) -> Vec<u32> {
    let mut key_size_scores: Vec<(u32, f64)> = Vec::new();
    for keysize in 2..=40 {
        let chunk_1 = scoring::StringOrBytes::ByteArray(&message[..keysize]);
        let chunk_2 = scoring::StringOrBytes::ByteArray(&message[keysize..(keysize * 2)]);
        let chunk_3 = scoring::StringOrBytes::ByteArray(&message[(keysize * 2)..(keysize * 3)]);
        let chunk_4 = scoring::StringOrBytes::ByteArray(&message[(keysize * 3)..(keysize * 4)]);

        key_size_scores.push(
            (
                keysize as u32,
                (scoring::hamming_distance(&chunk_1, &chunk_2)
                + scoring::hamming_distance(&chunk_1, &chunk_3)
                + scoring::hamming_distance(&chunk_1, &chunk_4)
                + scoring::hamming_distance(&chunk_2, &chunk_3)
                + scoring::hamming_distance(&chunk_2, &chunk_4)
                + scoring::hamming_distance(&chunk_3, &chunk_4)
                ) as f64 / (6.0 * (keysize as f64))
            )
        );
    }
    key_size_scores.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap());
    key_size_scores.into_iter().map(|(x, _)| x).collect()
}

fn transpose_message_chunks(mut message_chunks: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    /* Pointer magic could probably clean this up and make it considerably faster,
    but it should be a one time operation and not an issue unless the input text is large. */
    let mut transposed_chunks: Vec<Vec<u8>> = Vec::new();
    let message_chunk_0_length = message_chunks[0].len() as u32;
    for _ in 0..message_chunk_0_length {
        let message_iter = message_chunks.clone().into_iter();
        let mut row: Vec<u8> = Vec::new();
        for (index, _) in message_iter.enumerate() {
            if message_chunks[index].len() > 0 {
                row.push(message_chunks[index].remove(0));
            }
        }
        transposed_chunks.push(row);
    }
    transposed_chunks
}

pub fn break_repeating_key_xor(message: Bytes, result_count: u64) -> (Vec<String>, Vec<Bytes>) {
    let keysizes = get_repeat_xor_likely_key_size(&message);
    let mut decrypted_messages: Vec<Bytes> = Vec::new();
    let mut keys: Vec<String> = Vec::new();
    for keysize in keysizes[..(result_count as usize)].into_iter() {
        let message_chunks: Vec<Vec<u8>> = message.data.chunks(*keysize as usize).map(|x| x.to_vec()).collect();
        let transposed_chunks = transpose_message_chunks(message_chunks);
        let mut decrypted_chunks_transposed: Vec<Vec<u8>> = Vec::new();
        let mut key: Vec<char> = Vec::new();
        for chunk in transposed_chunks.iter() {
            let (key_char, _, decrypted_chunk)
                = crack_single_byte_xor(&Bytes::from_vec(chunk.clone()));
            key.push(key_char);
            decrypted_chunks_transposed.push(decrypted_chunk.data);
        }
        decrypted_messages.push(
            Bytes::from_vec(
                transpose_message_chunks(decrypted_chunks_transposed)
                .into_iter()
                .reduce(|acc, current| [acc, current].concat())
                .unwrap()
            )
        );
        keys.push(key.into_iter().collect());
    }
    (keys, decrypted_messages)
}

pub fn read_file_lines<P>(filename: P) ->io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

mod tests {
    use crate::bytes::bytes::Bytes;
    use super::*;
    #[test]
    fn single_byte_xor_crack() {
        let input_hex_message_string = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";
        let input_message = Bytes::from_hex_str(input_hex_message_string);
        let (key, _, decoded_message) = crack_single_byte_xor(&input_message);
        let decoded_string = decoded_message
                                        .to_ascii_string()
                                        .unwrap();
        println!("{key}, {decoded_string}");
        assert_eq!("Cooking MC's like a pound of bacon", decoded_string);
    }

    #[test]
    fn test_find_single_char_xor_in_file() {
        let (key, score, message_bytes) = find_single_char_xor_in_file("resources/4.txt");
        let message = message_bytes.to_ascii_string().unwrap();
        println!("{key}: {message} ({score})")
    }

    #[test]
    fn break_repeated_key_xor_from_file() {
        let text_lines = match read_file_lines("resources/6.txt") {
            Ok(text_lines) => {
                let message = text_lines
                    .into_iter()
                    .reduce(|acc, current| Ok([acc.unwrap(), current.unwrap()].concat()))
                    .unwrap().unwrap();
                Bytes::from_b64_str(&message)
            },
            Err(_) => panic!("Failed to read file resources/6.txt")
            };
        let (keys, messages)
            = break_repeating_key_xor(text_lines.clone(), 1);
        for (key, message_bytes) in keys.into_iter().zip(messages) {
            let message = message_bytes.to_ascii_string().unwrap();
            // println!("{key}: {message}");
            assert_eq!(key, "Terminator X: Bring the noise")
        }
    }
}