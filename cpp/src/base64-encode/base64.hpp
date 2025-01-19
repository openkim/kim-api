// from https://www.boost.org/doc/libs/1_87_0/boost/beast/core/detail/base64.hpp
// distributed under the same License
#ifndef BASE64_HPP
#define BASE64_HPP

#include <cctype>
#include <string>
#include <utility>

namespace base64
{


/// \brief Returns max chars needed to encode a base64 string
///
/// \param[in] n Size of char string
/// \return max size of encoded string generated from n chars
inline std::size_t encoded_size(std::size_t n)
{
  return 4 * ((n + 2) / 3);
}

/// \brief Returns max bytes needed to decode a base64 string
///
/// \param[in] n Size of base64 string
/// \return size of char string form n length base64 string
inline std::size_t decoded_size(std::size_t n)
{
  return n / 4 * 3;  // requires n&3==0, smaller
}

static const int MAX_BASE64_WIDTH = 76;
static const unsigned int IO_CHUNK = 1024 * 48;
static const int MAX_BINARY_WIDTH = static_cast<int>(MAX_BASE64_WIDTH / 4 * 3);

/// \brief get valid character
/// \return valid char
inline char const * get_alphabet()
{
  static char const tab[] = {"ABCDEFGHIJKLMNOP"
                             "QRSTUVWXYZabcdef"
                             "ghijklmnopqrstuv"
                             "wxyz0123456789+/"};
  return &tab[0];
}

inline signed char const * get_inverse()
{
  static signed char const tab[] = {
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  //   0-15
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  //  16-31
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, 62, -1, -1, -1, 63,  //  32-47
      52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, -1, -1, -1, -1, -1, -1,  //  48-63
      -1, 0,  1,  2,  3,  4,  5,  6,
      7,  8,  9,  10, 11, 12, 13, 14,  //  64-79
      15, 16, 17, 18, 19, 20, 21, 22,
      23, 24, 25, -1, -1, -1, -1, -1,  //  80-95
      -1, 26, 27, 28, 29, 30, 31, 32,
      33, 34, 35, 36, 37, 38, 39, 40,  //  96-111
      41, 42, 43, 44, 45, 46, 47, 48,
      49, 50, 51, -1, -1, -1, -1, -1,  // 112-127
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 128-143
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 144-159
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 160-175
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 176-191
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 192-207
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 208-223
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1,  // 224-239
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1  // 240-255
  };
  return &tab[0];
}

/// \brief Encode a series of octets as a padded, base64 string.
/// The resulting string will not be null terminated.
/// The memory pointed to by `out` points to valid memory
/// of at least `encoded_size(len)` bytes.
///
/// \param[in] dest pointer to location where characters are to be stored
/// \param[in] src pointer to source data
/// \param[in] len length of input string
/// \return The number of characters written to `out`. This
///         will exclude any null termination.
inline std::size_t encode(char * dest, char const * src, std::size_t len)
{
  char * out = dest;
  char const * in = src;
  const char * tab = base64::get_alphabet();

  for (size_t n = len / 3; n > 0; n--)
  {
    *out++ = tab[(in[0] & 0xfc) >> 2];
    *out++ = tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
    *out++ = tab[((in[2] & 0xc0) >> 6) + ((in[1] & 0x0f) << 2)];
    *out++ = tab[in[2] & 0x3f];
    in += 3;
  }

  switch (len % 3)
  {
    case 2:
      *out++ = tab[(in[0] & 0xfc) >> 2];
      *out++ = tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
      *out++ = tab[(in[1] & 0x0f) << 2];
      *out++ = '=';
      break;

    case 1:
      *out++ = tab[(in[0] & 0xfc) >> 2];
      *out++ = tab[((in[0] & 0x03) << 4)];
      *out++ = '=';
      *out++ = '=';
      break;

    case 0: break;
  }

  return out - static_cast<char *>(dest);
}

/// \brief Decode a padded base64 string into a series of octets.
/// The memory pointed to by `out` points to valid memory
/// of at least `decoded_size(len)` bytes.
///
/// \param[in] dest
/// \param[in] src
/// \param[in] len
/// \return  The number of octets written to `out`, and
///          the number of characters read from the input string,
///          expressed as a pair.
inline std::pair<std::size_t, std::size_t>
decode(char * dest, unsigned char const * src, std::size_t len)
{
  char * out = dest;
  unsigned char const * in = src;
  unsigned char c3[3], c4[4] = {0, 0, 0, 0};
  int i = 0;
  int j = 0;

  const signed char * inverse = base64::get_inverse();

  while (len-- && *in != '=')
  {
    const signed char v = inverse[*in];
    if (v == -1) break;
    ++in;
    c4[i] = v;
    if (++i == 4)
    {
      c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
      c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
      c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

      for (i = 0; i < 3; i++) *out++ = c3[i];
      i = 0;
    }
  }

  if (i)
  {
    c3[0] = (c4[0] << 2) + ((c4[1] & 0x30) >> 4);
    c3[1] = ((c4[1] & 0xf) << 4) + ((c4[2] & 0x3c) >> 2);
    c3[2] = ((c4[2] & 0x3) << 6) + c4[3];

    for (j = 0; j < i - 1; j++) *out++ = c3[j];
  }

  std::pair<std::size_t, std::size_t> result;
  result.first = out - static_cast<char *>(dest);
  result.second = in - reinterpret_cast<unsigned char const *>(src);
  return result;
}

}  // namespace base64

#endif  // BASE64_HPP
