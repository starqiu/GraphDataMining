#include "gdm.h"

using namespace std;

int main(){
	string in_file_path(BASE_PATH);
	//in_file_path +="liver_labeled_data.txt";  
	in_file_path +="example.txt";  
	string out_file_path(BASE_PATH);
	out_file_path +="output.txt"; 
	divide_files_by_periods(in_file_path);
	/*sd_test();
	dnb_test();
	generate_dnb();
	compare_to_example();*/
	return 0;
}
