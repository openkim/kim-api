/********************************************************************************
 *
 * report_error function
 *
 ********************************************************************************/
static void report_error(int line, char* str, int status)
{
   printf("Error at line %i of %s: %s with status = %i.\n",line,__FILE__,str,status);
   exit(-1);
}
