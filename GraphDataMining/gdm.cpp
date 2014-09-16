#include  <stdio.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include "gdm.h"

using namespace std;

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
	const char * file_name = "example.txt"; 
	char file_path[40];
	strcpy(file_path,BASE_PATH);
	strcat(file_path,file_name);
	//divide_files_by_periods();
	//cout<<"helllo "<<endl;
	DataFrame* df = new DataFrame();
	read_table(file_path,df,' ');
	cout<<df->getRows()[0][0]<<endl;

	char out_file_path[40];
	strcpy(out_file_path,BASE_PATH);
	strcat(out_file_path,"output.txt");
	write_table(out_file_path,df,',');
	/*sd_test();
	dnb_test();
	generate_dnb();
	compare_to_example();*/
	return 0;
}
