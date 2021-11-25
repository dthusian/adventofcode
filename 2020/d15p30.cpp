#include<iostream>
#include<vector>
#include<string>
#include<fstream>
#include<sstream>
#include<unordered_map>

struct dpsobj {
  int old, neww;
  constexpr dpsobj() : old(-1), neww(-1) { }
};

std::unordered_map<int, dpsobj> mem;
int last;

void pushmem(int k, int i) {
  dpsobj& c = mem[k];
  c.old = c.neww;
  c.neww = i;
}

constexpr int epoch = 30000000;

// String ops

template<class Out>
Out cast_str(const std::string& str) = delete;

template<>
int cast_str(const std::string& str) {
  return std::stoi(str);
}

template<class T>
std::vector<T> split(std::string s, char c) {
  std::vector<T> t;
  size_t last = 0;
  while (true) {
    size_t index = s.find_first_of(c, last);
    if (index == s.npos) {
      std::string tmp = s.substr(last);
      t.push_back(cast_str<int>(tmp));
      break;
    }
    std::string tmp = s.substr(last, index);
    t.push_back(cast_str<int>(tmp));
    last = index + 1;
  }
  return t;
}

// main

int main() {
  // Read input
  std::ifstream ifstr("input.txt", std::ios::in);
  std::string str;
  ifstr >> str;
  ifstr.close();
  std::vector<int> input = split<int>(str, ',');
  // Preproc input
  for (int i = 0; i < input.size(); i++) {
    pushmem(input[i], i);
    last = input[i];
  }
  // dp
  for (int i = input.size(); i < epoch; i++) {
    if (i % 1000000 == 0) std::cout << i << "\n";
    int next;
    if (mem[last].old == -1) {
      next = 0;
    }
    else {
      next = mem[last].neww - mem[last].old;
    }
    pushmem(next, i);
    last = next;
  }
  std::cout << last << "\n";
}