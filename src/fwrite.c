#include <R.h>
#include <errno.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>

/*
 * Forward declarations can allow access to the private API
 * Might not work on some platforms.
 */
Rconnection getConnection(int);
int Rconn_printf(Rconnection con, const char *format, ...);

/* custom helpers */
inline static int Rconn_puts(const char *str, Rconnection con) {
  return Rconn_printf(con, "%s", str);
}

inline static int Rconn_putc(char c, Rconnection con) {
  /* must use this for some connections such as stdout */
  return Rconn_printf(con, "%c", c);
  
  /* this works for normal files and would be faster */
  /* return (con->write)(&c, 1, 1, con); */
}

void writefile(SEXP list_of_columns,
               SEXP connection_exp,
               SEXP col_sep_exp,
               SEXP row_sep_exp,
               SEXP na_exp,
               SEXP quote_cols,
               SEXP qmethod_escape_exp) {

  Rconnection f = getConnection(asInteger(connection_exp));
  
  int error_number = 0;
  int qmethod_escape = *LOGICAL(qmethod_escape_exp);
  
  errno = 0; /* clear flag possibly set by previous errors */
  
  char col_sep = *CHAR(STRING_ELT(col_sep_exp, 0));
  const char *row_sep = CHAR(STRING_ELT(row_sep_exp, 0));
  const char *na_str = CHAR(STRING_ELT(na_exp, 0));
  const char QUOTE_CHAR = '"';
  const char ESCAPE_CHAR = '\\';

  
  R_xlen_t ncols = LENGTH(list_of_columns);
  R_xlen_t nrows = LENGTH(VECTOR_ELT(list_of_columns, 0));
  
  for (R_xlen_t row_i = 0; row_i < nrows; ++row_i) {
    for (int col_i = 0; col_i < ncols; ++col_i) {
      
      if (col_i > 0) Rconn_putc(col_sep, f);
      
      SEXP column = VECTOR_ELT(list_of_columns, col_i);
      
      switch(TYPEOF(column)) {
      case INTSXP:
        if (INTEGER(column)[row_i] == NA_INTEGER) Rconn_puts(na_str, f);
        else Rconn_printf(f, "%d", INTEGER(column)[row_i]);
        break;
      
      case REALSXP:
        if (ISNA(REAL(column)[row_i])) Rconn_puts(na_str, f);
        else Rconn_printf(f, "%.15g", REAL(column)[row_i]);
        break;
      
      default: /* assuming STRSXP */
        if (STRING_ELT(column, row_i) == NA_STRING) Rconn_puts(na_str, f);
        else {
          int quote = LOGICAL(quote_cols)[col_i];
          if (quote) Rconn_putc(QUOTE_CHAR, f);
          for (const char *ch = CHAR(STRING_ELT(column, row_i)); *ch != '\0'; ++ch) {
            if (quote) {
              if (*ch == QUOTE_CHAR) {
                if (qmethod_escape) Rconn_putc(ESCAPE_CHAR, f);
                else Rconn_putc(QUOTE_CHAR, f); /* qmethod = "double" */
              }
              if (qmethod_escape && *ch == ESCAPE_CHAR) Rconn_putc(ESCAPE_CHAR, f);
            }
            Rconn_putc(*ch, f);
          }
          if (quote) Rconn_putc(QUOTE_CHAR, f);
        }
        break;
      }
    }
    if (Rconn_puts(row_sep, f) < 0) {
      error(strerror(errno));
      return;
    }
  }
}