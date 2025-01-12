// ASK RYAN:
// License for this file? The original class was part of Boost, and distributed under
// the Boost Software License. It allows for inclusion of the code in other projects
// so no issues there, but Boost Licence explicitly asks that the Boost License
// must be included along with the LGPL-2.1-or-later license. Is it sufficient to
// include the Boost License in the header file as well, or will it need to be included
// in base project as well?
// Rewrite the class? Though it will be similar to current version with minor changes

// Currently distributed under Boost Software License

#ifndef BASE64_HPP
#define BASE64_HPP

#include <cstddef>
#include <string>
// #include <cstdint>

namespace Base64 {
  // Based on boost https://www.boost.org/doc/libs/1_66_0/boost/beast/core/detail/base64.hpp
 namespace
 {
  /// \brief Base64 alphabet table
  inline const char* get_alphabet() {
    static const char tab[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    return tab;
  }

  /// \brief Base64 inverse lookup table
  inline static const signed char* get_inverse() {
    static const signed char tab[] = {
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
        -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
        -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
    };
    return tab;
  }
 } // anonymous namespace

  /// \brief Calculate encoded size
  ///
  /// \param n The size of the input
  /// \return The size of the encoded output
  inline static size_t encoded_size(size_t n) {
    return 4 * ((n + 2) / 3);
  }

  /// \brief Calculate maximum decoded size
  ///
  /// \param n The size of the input
  /// \return The maximum size of the decoded output
  inline static  size_t decoded_size(size_t n) {
    return n / 4 * 3;
  }

  // width of encoded output
   const static std::size_t MAX_BASE64_WIDTH = 76;
   const static std::size_t MAX_BINARY_WIDTH = MAX_BASE64_WIDTH/4 * 3;


  /// \brief Encode a string
  ///
  /// \param input The input string
  /// \return The encoded string
  inline static std::string encode(const std::string& input) {
    std::string output;
    output.resize(encoded_size(input.size()));

    const char* in = input.data();
    char* out = &output[0];
    size_t len = input.size();
    const char * tab = get_alphabet();

    // Process 3-byte chunks
    for(size_t n = len / 3; n--;) {
      *out++ = tab[(in[0] & 0xfc) >> 2];
      *out++ = tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
      *out++ = tab[((in[2] & 0xc0) >> 6) + ((in[1] & 0x0f) << 2)];
      *out++ = tab[in[2] & 0x3f];
      in += 3;
    }

    // Handle remaining bytes
    switch(len % 3) {
      case 2:
        *out++ = tab[(in[0] & 0xfc) >> 2];
        *out++ = tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
        *out++ = tab[(in[1] & 0x0f) << 2];
        *out++ = '=';
        break;
      case 1:
        *out++ = tab[(in[0] & 0xfc) >> 2];
        *out++ = tab[(in[0] & 0x03) << 4];
        *out++ = '=';
        *out++ = '=';
        break;
    }

    output.resize(out - &output[0]);
    return output;
  }

  /// \brief Encode a string, counterpart to encode
  ///
  /// \param input The input string
  /// \return decoded string
  inline static std::string decode(const std::string& input) {
    std::string output;
    output.resize(decoded_size(input.size()));

    const unsigned char* in = reinterpret_cast<const unsigned char*>(input.data());
    char* out = &output[0];
    size_t len = input.size();

    unsigned char c3[3], c4[4];
    int i = 0;
    int j = 0;

    const signed char * inverse = get_inverse();

    while(len-- && *in != '=') {
      const signed char v = inverse[*in];
      if(v == -1)
        break;
      ++in;
      c4[i] = v;
      if(++i == 4) {
        c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
        c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
        c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

        for(i = 0; i < 3; i++)
          *out++ = c3[i];
        i = 0;
      }
    }

    if(i) {
      c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
      c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
      c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

      for(j = 0; j < i - 1; j++)
        *out++ = c3[j];
    }

    output.resize(out - &output[0]);
    return output;
  }

  /// \brief Decode a string, but with a more C-like interface
  ///
  /// \param input pointer to input buffer
  /// \param len_in length of the input buffer
  /// \param output pointer to store the output buffer
  /// \param len_out length of the output buffer written
  inline static void decode(unsigned char const * input,
                            const std::size_t len_in,
                            unsigned char * const output,
                            std::size_t& len_out) {
    std::size_t len = len_in;
    unsigned char* out = output;
    unsigned char * in = const_cast<unsigned char*>(input);

    unsigned char c3[3], c4[4];
    int i = 0;
    int j = 0;

    const signed char * inverse = get_inverse();

    while(len-- && *in != '=') {
      const signed char v = inverse[*in];
      if(v == -1)
        break;
      ++in;
      c4[i] = v;
      if(++i == 4) {
        c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
        c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
        c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

        for(i = 0; i < 3; i++)
          *out++ = c3[i];
        i = 0;
      }
    }

    if(i) {
      c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
      c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
      c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

      for(j = 0; j < i - 1; j++)
        *out++ = c3[j];
    }

    len_out = out - output;
  }

}

#endif // BASE64_HPP
