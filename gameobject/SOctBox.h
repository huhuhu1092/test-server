#ifndef SOCTBOX_H
#define SOCTBOX_H
#include "SMath.h"
#include <vector>
using namespace std;
/*
 *
 * ---- |-----
 * | 0  | 1   |
 * |----|-----|
 * | 2  | 3   |
 * |----|-----|
 *
 * ---- |-----
 * | 4  | 5   |
 * |----|-----|
 * | 6  | 7   |
 * |----|-----|

 *
 * */
// right > left, top > bottom, near > far
// this is for right hand coordinate
class SOctBox
{
public:
    SOctBox();
    SOctBox(Real l, Real r, Real t, Real b, Real n, Real f);
    ~SOctBox();
    SOctBox* getChild(int index)
    {
        return mChildren[index];
    }
    Real getXExt()
    {
        return right - left;
    }
    Real getYExt()
    {
        return top - bottom;
    }
    Real getZExt()
    {
        return near - far;
    }
    Real getLeft()
    {
        return left;
    }
    Real getRight()
    {
        return right;
    }
    Real getTop()
    {
        return top;
    }
    Real getBottom()
    {
        return bottom;
    }
    Real getNear()
    {
        return near;
    }
    Real getFar()
    {
        return far;
    }
    int getChildCount()
    {
        return mChildren.size();
    }
private:
    vector<SOctBox*> split();
private:
    //right > left, top > bottom, near > far
    //this is right hand coordinate
    Real left, right, top, bottom, near,far;
    vector<SOctBox*> mChildren;
}; 
#endif
