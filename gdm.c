#include  <stdio.h>
#include "gdm.h"

void divide_files_by_periods();
void calc_and_filter_sd (char* file_name);
void sd_test();
void calc_pcc (char* file_name);
double calc_ci (double pccin,double pccout,double sd);
void pcc_test (char* file_name);
void dnb_test ();
void generate_dnb ();
void compare_to_example();

int main(){
	divide_files_by_periods();
	/*sd_test();
	dnb_test();
	generate_dnb();
	compare_to_example();*/
	return 0;
}
