#include <iostream>
#include <stack>
#include <utility>

#define intpair pair<int, int>

using namespace std;

int main() {
  int children, meta, value, sum = 0;
  stack<intpair> nodes;

  do {
    if (!nodes.empty() && nodes.top().first == 0) {
      for (int i = 0; i < nodes.top().second; ++i) {
        cin >> value;
        sum += value;
      }

      nodes.pop();
    } else {
      if (!nodes.empty()) {
        nodes.top().first--;
      }

      cin >> children >> meta;
      nodes.push(intpair(children, meta));
    }
  } while (!nodes.empty());

  cout << sum << endl;

  return 0;
}
