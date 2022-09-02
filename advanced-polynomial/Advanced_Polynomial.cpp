// use C++11 because of lambda experessions

#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <string>
#include <algorithm>
#include <limits>

using namespace std;

std::fstream& GotoLine(std::fstream& file, unsigned int num){
    file.seekg(std::ios::beg);
    for(int i=0; i < num - 1; ++i){
        file.ignore(std::numeric_limits<std::streamsize>::max(),'\n');
    }
    return file;
}

const int ERROR = 2019;

class Term;

class Polynomial{
	public :
		Polynomial (string input);
		Polynomial operator+ (Polynomial to_add)const;
		Polynomial operator- (Polynomial to_subtract)const;
		Polynomial operator* (Polynomial to_mult);
		void operator+=(Polynomial plus_shorthand);
		void operator+=(float &plus_shorthand);
		void operator-=(Polynomial minus_shorthand);
		void operator-=(float &minus_shorthand);
		void operator*=(Polynomial mult_shorthand);
		void operator*=(float &mult_shorthand);
		Polynomial operator %(char derivate_by);
		Polynomial integral(char factor)const;
		friend ostream &operator<<(ostream &output, const Polynomial &P);
		int find_degree();
		float calculate();// complete this
	private:
		vector<Term> solvable;
		Polynomial (vector<Term> vec);
		void organize();
};

class Term{
	public :
		Term operator* (Term to_mult);
		Term operator* (float &to_mult) const;////
		void operator*=(const Term &mult_shorthand);
		void operator*=(float &mult_shorthand);////
		void operator= (const Term &to_assign);
		void operator= (float &to_assign);////
		bool operator> (const Term &to_compare)const;
		bool operator< (const Term &to_compare)const;
		bool operator<=(const Term &to_compare)const;
		bool operator>=(const Term &to_compare)const;
		bool operator==(const Term &to_compare)const;
		bool operator!=(const Term &to_compare)const;
		Term operator %(char derivate_by)const;
		Term integral(char factor)const;
		friend ostream &operator<<(ostream &output, const Term &T);
		
		friend class Polynomial;
	private:
		Term (string input);
		Term (float _coeff, vector<pair<char, int>> var);
		void organize();
		float coefficient;
		vector <pair<char, int>> variables;
};

void Term :: organize(){
	for(int a = 0; a < variables.size(); a++){
		for(int b = a+1 ; b < variables.size(); b++){
			if(variables[a].first == variables[b].first){
				variables[a].second = variables[a].second + variables[b].second;
				variables.erase(variables.begin() + b);
				b--;
			}
		}
	}
	sort(variables.begin() , variables.end(), [](pair<char, int> &a , pair<char, int> &b){return a.first < b.first;});
}

Term :: Term(string input){
	int signal = input.length();
	for(int i = 0; i < input.length(); i++){
		if(input[i] == '-' || input[i] == '+' || input[i] == '.')
			continue;
		if(!isdigit(input[i])){
			signal = i;
			break;
		}
	}
	if(signal >= 2)
		coefficient = stof(input.substr(0, signal));
	else if(input[0] == '-')
		coefficient = -1;
	else
		coefficient = 1;
	for(int i = 0; i < input.length(); i++){
		if((input[i] > 64 && input[i] < 91) || (input[i] > 96 && input[i] < 123)){
			if(input[i+1] != '^'){
				variables.push_back(make_pair(input[i], 1));
			}
			else if(input[i+1] == '^'){
				int arrow = input.length();
				for(int j = i + 2 ; j < input.length() ; j++){
					if(!isdigit(input[j])){
						arrow = j;
						break;
					}
				}
				string _power = input.substr(i+2, arrow);
				int str_to_int_power = stoi(_power);
				variables.push_back(make_pair(input[i], str_to_int_power));
				i++;
			}
		}
	}
	organize();
}

Term :: Term(float _coeff, vector<pair<char, int>> var){
	coefficient = _coeff;
	variables = var;
	organize();
}

Term Term :: operator*(Term to_mult){
	to_mult.variables.insert(to_mult.variables.end(), variables.begin(), variables.end());
	to_mult.organize();
	to_mult.coefficient *= coefficient;
	return to_mult;
}

Term Term :: operator*(float &to_mult)const{
	return Term(coefficient*to_mult, variables);
}

void Term :: operator*=(const Term &mult_shorthand){
	*this = *this * mult_shorthand;
}

void Term :: operator*=(float &mult_shorthand){
	coefficient *= mult_shorthand;
}

void Term :: operator=(const Term &to_assign){
	coefficient = to_assign.coefficient;
	variables = to_assign.variables;
}

void Term :: operator=(float &to_assign){
	coefficient = to_assign;
	variables.clear();
}

bool Term :: operator> (const Term &to_compare)const{
	int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 > sum_degree2);
}

bool Term :: operator< (const Term &to_compare)const{
		int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 < sum_degree2);
}

bool Term :: operator<=(const Term &to_compare)const{
		int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 <= sum_degree2);
}

bool Term :: operator>=(const Term &to_compare)const{
		int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 >= sum_degree2);
}

bool Term :: operator==(const Term &to_compare)const{
		int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 == sum_degree2);
}

bool Term :: operator!=(const Term &to_compare)const{
		int sum_degree1 = 0, sum_degree2 = 0;
	for(int i = 0; i < variables.size(); i++){
		sum_degree1 += variables[i].second;
	}
	for(int j = 0; j < to_compare.variables.size(); j++){
		sum_degree2 += to_compare.variables[j].second;
	}
	return (sum_degree1 != sum_degree2);
}

Term Term :: operator %(char derivate_by)const{
	for(int i = 0 ; i < variables.size() ; i++){
		if(derivate_by == variables[i].first){
			if(variables[i].second > 1 || variables[i].second < 0){
				float coeff = coefficient * variables[i].second;
				vector<pair<char, int>> var = variables;
				var[i].second--;
				return Term (coeff, var);
			}
			vector<pair<char, int>> var = variables;
			var.erase(var.begin()+ i);
			return Term(coefficient, var);
		}
	}
	throw ERROR;
}

ostream &operator<<(ostream &output, const Term &T){
	if(T.coefficient > 0)
		output<<'+'<<T.coefficient;
	else if(T.coefficient < 0)
		output<<T.coefficient;
	for(int i = 0 ; i < T.variables.size(); i++){
		output<<T.variables[i].first;
		if(T.variables[i].second != 1)
			output<<'^'<<'('<<T.variables[i].second<<')';
	}
	return output;
}

Term Term :: integral(char factor)const{
	vector<pair<char, int>> var = variables;
	for(int i = 0; i <variables.size(); i++){
		if(variables[i].first == factor && variables[i].second != -1){
			var[i].second += 1;
			return Term(coefficient/var[i].second, var);
		}
		else if(variables[i].first == factor && variables[i].second == -1)
			throw ERROR;
	}
	var.push_back(make_pair(factor, 1));
	return Term(coefficient, var);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Polynomial :: organize(){
	for(int i = 0; i < solvable.size(); i++){
		for(int j = i + 1; j < solvable.size(); j++){
			if(solvable[i].variables == solvable[j].variables){
				solvable[i].coefficient = solvable[i].coefficient + solvable[j].coefficient;
				solvable.erase(solvable.begin() + j);
				j--;
			}
		}
	}
	sort(solvable.begin() , solvable.end(), [](Term &a , Term &b){return a > b;});
}

Polynomial :: Polynomial (string input){
	int signal = 0;
	string gravel = "[]{} ()*";
	for(int i = 0; i < gravel.length(); i++)
		input.erase(remove(input.begin(), input.end(), gravel[i]), input.end());
	replace( input.begin(), input.end(), '_', '-');
	if(input[0] != '-' && input[0] != '+')
		input.insert(0,1,'+');
	input = input + "+";
	for(int i = 1; i < input.length(); i++){
		if(input[i] == '+' || input[i] == '-'){
			if(input[i-1] == '^')
				continue;
			string temp = input.substr(signal, i - signal);
			solvable.push_back(Term(temp));
			signal = i;
		}
	}
	organize();
}


Polynomial :: Polynomial(vector<Term> vec){
	solvable = vec;
	organize();
}

Polynomial Polynomial :: operator+ (Polynomial to_add) const{
	to_add.solvable.insert(to_add.solvable.end(), solvable.begin(), solvable.end());
	return Polynomial(to_add.solvable);
}

Polynomial Polynomial :: operator- (Polynomial to_subtract) const{
	for(int i = 0; i < to_subtract.solvable.size(); i++){
		to_subtract.solvable[i].coefficient = (to_subtract.solvable[i].coefficient * -1);
	}
	return (*this + to_subtract);
}

Polynomial Polynomial :: operator* (Polynomial to_mult){
	vector <Term> temp;
	for(int i = 0 ; i < solvable.size() ; i++){
		for(int j = 0 ; j < to_mult.solvable.size(); j++){
			temp.push_back(solvable[i] * to_mult.solvable[j]);
		}
	}
	return Polynomial (temp);
	
}

void Polynomial :: operator+=(Polynomial plus_shorthand){
	solvable.insert(solvable.end(), plus_shorthand.solvable.begin(), plus_shorthand.solvable.end());
	organize();
}

void Polynomial :: operator+=(float &plus_shorthand){
	solvable.push_back(Term(to_string(plus_shorthand)));
	organize();
}

void Polynomial :: operator-=(Polynomial minus_shorthand){
	for(int i = 0; i < minus_shorthand.solvable.size(); i++){
		minus_shorthand.solvable[i].coefficient = (minus_shorthand.solvable[i].coefficient * -1);
	}
	solvable.insert(solvable.end(), minus_shorthand.solvable.begin(), minus_shorthand.solvable.end());
	organize();
}

void Polynomial :: operator-=(float &minus_shorthand){
	minus_shorthand *= -1;
	solvable.push_back(Term(to_string(minus_shorthand)));
	organize();
}

void Polynomial :: operator*=(Polynomial mult_shorthand){
 *this = *this * mult_shorthand;
}

void Polynomial :: operator*=(float &mult_shorthand){
	for(int i = 0; i < solvable.size(); i++){
		solvable[i].coefficient *= mult_shorthand;
	}
}

Polynomial Polynomial :: operator %(char derivate_by){
	vector<Term> temp;
	for(int i = 0; i < solvable.size(); i++){
		try{
			temp.push_back(solvable[i] % derivate_by);
		}
		catch(int num){
			solvable.erase(solvable.begin() + i);
			i -= 1;
		}
	}
	return Polynomial(temp);
}

Polynomial Polynomial :: integral(char factor)const{
	vector <Term> temp;
	for(int i = 0; i < solvable.size() - 1; i++){
		try{
			temp.push_back(solvable[i].integral(factor));
		}
		catch(int num){
			cout<<"\n\n sorry I can't solve this right now..";
			sleep(2);
			abort();
		}
	}
	temp.push_back(Term("1"));
	return Polynomial(temp);
}

ostream &operator<<(ostream &output,const Polynomial &P){
	for(int i = 0; i < P.solvable.size(); i++){
		output << P.solvable[i]<<' ';
	}
	return output;
}

int Polynomial :: find_degree(){
	Term max_term(" ");
	int max_degree;
	for(int i = 0; i < solvable.size(); i++){
		if(solvable[i] > max_term)
			max_term = solvable[i];
	}
	for(int i = 0; i < max_term.variables.size(); i++){
		max_degree += max_term.variables[i].second;
	}
	return max_degree;
}

int main(){
	Polynomial current("0");
	Polynomial previous("0");
	int i = 0;
	int choice;
	string inputted;
	while(true){
		cout<<"\n\n MAIN MENU  process "<<++i<<"\n\n  1- New Polynomial\n\n"<<
		"  2- Load from text file\n\n  3- Load from binary file\n\n  4- Quit "<<
		"\n\n -> ";
		cin>>choice;
		switch(choice){
			case 1:{
				while(true){
					cout<<"\n\n POLYNOMIAL MENU"<<"\n\n Previous Polynomial : "<<previous<<"\n\n Current Polynomial : "<<current<<
					"\n\n  1- Add\n\n  2- Subtract\n\n  3- Multiply\n\n  4- Derivative"<<
					"\n\n  5- Integral\n\n  6- Find Degree\n\n  7- Find value for specific variables"<<
					"\n\n  8- Save to a text file\n\n  9- Save to a binary file"<<
					"\n\n  10- Back to main menu\n\n -> ";
					cin>>choice;
					switch(choice){
						case 1:{
							cout<<"\n\n  Enter your Polynomial to add : ";
							cin.ignore();
							getline(cin, inputted);
							previous = current;
							current = previous + Polynomial(inputted);
							break;
						}
						case 2:{
							cout<<"\n\n  Enter your Polynomial to subtract : ";
							cin.ignore();
							getline(cin, inputted);
							previous = current;
							current = previous - Polynomial(inputted);
							break;
						}
						case 3:{
							cout<<"\n\n  Enter your Polynomial to multiply : ";
							cin.ignore();
							getline(cin, inputted);
							previous = current;
							current = previous * Polynomial(inputted);
							break;
						}
						case 4:{
							cout<<"\n\n  What should I derive from? : ";
							char derive;
							cin>>derive;
							previous = current;
							current = current % derive;
							break;
						}
						case 5:{
							cout<<"\n\n  In terms of what should I get the integral? : ";
							char integrate;
							cin>>integrate;
							previous = current;
							current = current.integral(integrate);
							break;
						}
						case 6:{
							cout<<"\n\n  Polynomial's degree : "<<current.find_degree();
							break;
						}
						case 7:{
							cout<<"\n\n  under construction.. sorry :(";
							sleep(1);
							break;
						}
						case 8:{
							ofstream txtArchive;
							txtArchive.open("Archive.txt", ios::app);
							txtArchive << current<<endl;
							txtArchive.close();
							cout<<"\n\n  Polynomial saved in a txtfile successfully";
							sleep(2);
							break;
						}
						case 9:{
							ofstream binArchive( "Archive.bin", ios::app | ios::binary);
							binArchive << current<<endl;
							binArchive.close();
							cout<<"\n\n  Polynomial saved in a binfile successfully";
							sleep(2);
							break;
						}
						case 10:{
							current = Polynomial("0");
							previous= Polynomial("0");
							goto main_menu;
							break;
						}
						default:{
							cout<<"\n\n Invalid input.. try again";
							sleep(2);
							break;
						}
					}
				}
				break;
			}
			case 2:
			case 3:{
				while(true){
					cout<<"\n\n  Please enter the name of the file you want to read from : ";
					string file_name;
					cin >> file_name;
					fstream file_to_read;
					file_to_read.open(file_name, ios::in);
					if(file_to_read.is_open()){
						cout<<"\n\n What line do you want to be read ? ";
						int line;
						cin>>line;
						GotoLine(file_to_read, line);
						string poly;
						getline(file_to_read, poly);
						char trash[] = {')', '('};
						current = Polynomial(poly);
						break;
					}
				}
				break;
			}
			case 4:{
				cout<<"\n\n Ending myself . . .";
				sleep(1);
				abort();
				break;
			}
			default:
				cout<<"\n\nInvalid Input.. try again";
				sleep(1);
		}
		main_menu:;
	}
}