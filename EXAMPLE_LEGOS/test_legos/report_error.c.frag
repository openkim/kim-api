/********************************************************************************
 *
 * report_error function
 *
 ********************************************************************************/
static void report_error(int line, char* str, int status)
{
   printf("Error at line %i of %s: %s with status = %s.\n",
          line,__FILE__,str,KIM_API_status_msg(status));
   exit(-1);
}
