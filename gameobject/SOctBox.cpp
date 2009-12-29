#include "SOctBox.h"
SOctBox::SOctBox()
{
    left = right = top = bottom = far = near = 0.0;
    mChildren.resize(8, 0);
}
SOctBox::SOctBox(Real l, Real r, Real t, Real b, Real n, Real f)
{
    left = l;
    right = r;
    top = t;
    bottom = b;
    near = n;
    far = f;
    mChildren = split();
}

SOctBox::~SOctBox()
{
    for(int i = 0 ; i < mChildren.size() ; i++)
    {
        if(mChildren[i] != NULL)
            delete mChildren[i];
    }
}
vector<SOctBox*> SOctBox::split()
{
    vector<SOctBox*> splitedBox(8);
    for(int i = 0 ; i < 8 ; i++)
    {
        splitedBox[i] = new SOctBox();
    }
    Real midx = (left + right ) / 2;
    Real midY = (top + bottom ) / 2;
    Real midZ = (near + far) / 2;
    splitedBox[0]->left = left;
    splitedBox[0]->right = midX;
    splitedBox[0]->top = top;
    splitedBox[0]->bottom = midY;
    splitedBox[0]->near = midZ;
    splitedBox[0]->far = far;
    //////////////////////
    splitedBox[1]->left = midX;
    splitedBox[1]->right = right;
    splitedBox[1]->top = top;
    splitedBox[1]->bottom = midY;
    splitedBox[1]->near = midZ;
    splitedBox[1]->far = far;
    //////////
    splitedBox[2]->left = left;
    splitedBox[2]->right = midX;
    splitedBox[2]->top = top;
    splitedBox[2]->bottom = midY;
    splitedBox[2]->near = near;
    splitedBox[2]->far = midZ;
    //
    splitedBox[3]->left = midX;
    splitedBox[3]->right = right;
    splitedBox[3]->top = top;
    splitedBox[3]->bottom = midY;
    splitedBox[3]->near = near;
    splitedBox[3]->far = midZ;
    //
    splitedBox[4]->left = left;
    splitedBox[4]->right = midX;
    splitedBox[4]->top = midY;
    splitedBox[4]->bottom = bottom;
    splitedBox[4]->near = midZ;
    splitedBox[4]->far = far;
    //
    splitedBox[5]->left = midX;
    splitedBox[5]->right = right;
    splitedBox[5]->top = midY;
    splitedBox[5]->bottom = bottom;
    splitedBox[5]->near = midZ;
    splitedBox[5]->far = far;
    //
    splitedBox[6]->left = left;
    splitedBox[6]->right = midX;
    splitedBox[6]->top = midY;
    splitedBox[6]->bottom = bottom;
    splitedBox[6]->near = near;
    splitedBox[6]->far = midZ;
    //
    splitedBox[7]->left = midX;
    splitedBox[7]->right = right;
    splitedBox[7]->top = midY;
    splitedBox[7]->bottom = bottom;
    splitedBox[7]->near = near;
    splitedBox[7]->far = midZ;
    return splitedBox;
}

