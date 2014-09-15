#include <string>
#include <vector>

using namespace std;

class DataFrame{
private:
	vector<string> colNames;
	vector<vector<string>> rows;   
public:
	DataFrame(){}

	vector<string> getColNames(){
		return colNames;
	}
	void setColNames(vector<string> colNames){
		this->colNames = colNames;
	}
	vector<vector<string>> getRows(){
		return rows;
	}
	void setRows(vector<vector<string>> rows){
		this->rows = rows;
	}
	void pushRow(vector<string> row){
		this->rows.push_back(row);
	}

	~DataFrame(){}
};