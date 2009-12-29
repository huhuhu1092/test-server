#ifndef SOCTBOXTREE_H
#define SOCTBOXTREE_H
#include "SOctBox.h"
#include "SMath.h"
#include <vector>
using namespace std;
class SOctBoxNode
{
public:
    SOctBoxNode();
    virtual ~SOctBoxNode();
    SOctBox* box;
    vector<SOctBoxNode*> children;
};
class SOctBoxTree
{
public:
    // maxExt is the extent of the largest box
    // minExt is the extent of the smallest box which will stop recuring.
    SOctBoxTree(Real maxExt, Real minExt);
    virtual ~SOctBoxTree();
    void create();
    //for debug
    void print();
protected:
    virtual SOctBoxNode* createNode();
private:
    void print(SOctBoxNode* parent);
    SOctBoxNode* create(Real l, Real r, Real t, Real b, Real n, Real f, Real minExt);
private:
    Real mMaxExt;
    Real mMixExt;
    SOctBoxNode* mRoot;
};
#endif
