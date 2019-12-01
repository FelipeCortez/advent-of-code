#include <iostream>
#include <vector>
#include <stack>
#include <utility>

using namespace std;

struct node {
  int remaining;
  int metadataCount;
  vector<int> vals;
};

int main() {
  int children, meta, value, sum;
  stack<node> nodes;

  while (true) {
    if (!nodes.empty() && nodes.top().remaining == 0) {
      sum = 0;

      for (int i = 0; i < nodes.top().metadataCount; ++i) {
        cin >> value;

        if (nodes.top().vals.empty()) {
          sum += value;
        } else {
          if ((value - 1) < nodes.top().vals.size()) {
            sum += nodes.top().vals[value - 1];
          }
        }
      }

      nodes.pop();

      if (nodes.empty()) {
        cout << sum << endl;
        return 0;
      } else {
        nodes.top().vals.push_back(sum);
      }
    } else {
      if (!nodes.empty()) {
        nodes.top().remaining--;
      }

      cin >> children >> meta;
      node newNode = {children, meta, vector<int>()};
      nodes.push(newNode);
    }
  }
}
