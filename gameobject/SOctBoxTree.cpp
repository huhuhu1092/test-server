#include "SOctBoxTree.h"
SOctBoxNode::SOctBoxNode()
{
        
}
SOctBoxNode::~SOctBoxNode()
{
    if(box != NULL)
        delete box;
    if(children.empty())
        return;
    for(int i = 0 ; i < 8 ; i++)
    {
        if(children[i] != NULL)
            delete children[i];
    }
}

/////////////////////////////////////////////////////
SOctBoxTree::SOctBoxTree(Real maxExt, Real minExt)
{
    mMaxExt = maxExt;
    mMinExt = minExt;
    mRoot = 0;
}
SOctBoxTree::~SOctBoxTree()
{
    if(mRoot != NULL)
        delete mRoot;
}
void SOctBoxTree::create()
{
    mRoot = create(-mMaxExt, mMaxExt, -mMaxExt, mMaxext, -mMaxExt, mMaxExt, mMinExt);
}
void SOctBoxTree::print()
{
    print(mRoot);
}
void SOctBoxTree::print(SOctBoxNode* parent)
{
    if(parent == null)
        return;
    SLog::msg("l = %f, r = %f, t = %f, b = %f, n = %f, f = %f\n" ,parent.box.getLeft(), parent.box.getRight() , parent.box.getTop() , parent.box.getBottom() , parent.box.getNear(), parent.box.getFar());
    if(parent.children.empty())
        return;
    for(int i = 0 ; i < parent.children.length ; i++)
    {
        OctBoxNode child = parent.children[i];
        print(child);
    }
        
}
SOctBoxNode* SOctBoxTree::create(Real l, Real r, Real t, Real b, Real n, Real f, Real minExt)
{
    SOctBoxNode* parentNode = createNode();
    parentNode.box = new SOctBox(l, r, t, b, n, f);
    if(parentNode.box.getXExt() <= minExt)
        return parentNode;
    parentNode.children.resize(8);
    for(int i = 0 ; i < parentNode.children.size() ; i++)
    {
        SOctBox* box = parentNode.box.getChild(i);
        parentNode.children[i] = createBoxTree(box.getLeft(), box.getRight(), box.getTop(), box.getBottom(), box.getNear(), box.getFar(), minExt);
    }
    return parentNode;
}
SOctBoxNode* SOctBoxTree::createNode()
{
    return new SOctBoxNode;
}
