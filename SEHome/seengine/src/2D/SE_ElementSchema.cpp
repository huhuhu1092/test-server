#include "SE_ElementScheme.h"
#include "SE_Application.h"
#include "SE_Element.h"
#include "SE_ElementManager.h"
void SE_ElementSchema::createElement(SE_ElementManager* elementManager, SE_Element* parent)
{
    SE_Element* e = new SE_Element;
    e->setName(name.getStr());
    e->setFullPathName(fullPathName.getStr());
    e->setMountPointID(mountPointRef);
    e->setMountPoint(mountPointSet);
    e->setPivotX(pivotx);
    e->setPivotY(pivoty);
    e->setSeq(seq);
    elementManager->addElement(parent, e);
    if(parent == NULL)
        root = e;
    SE_ASSERT(children.empty() || contens.empty());
    _ElementContentList::iterator it;
    for(it = contents.begin() ; it != contents.end() ; it++)
    {
        SE_ElementContent* ec = *it;
        SE_Element* childElement = ec->createElement();
        elementManager->addElement(e, childElement);
    } 
    _ElementSchemaList::iterator itSchema;
    for(itSchema = children.begin() ; itSchema != children.end() ; itSchema++)
    {
        SE_ElementSchema* es = *itSchema;
        es->createElement(elementManager, e); 
    }
}
SE_Element* SE_ElementSchema::createElement()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    createElement(elementManager, NULL);
    elementManager->addElement(SE_ElementID::NULLID, root);
} 
