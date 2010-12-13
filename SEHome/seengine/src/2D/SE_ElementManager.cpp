#include "SE_ElementManager.h"
#include "SE_Log.h"
#include "SE_Element.h"
#include "SE_ElementGroup.h"
#include "SE_TextureCoordAnimation.h"
#include "SE_Common.h"
#include "SE_Spatial.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageMap.h"
#include "SE_ImageTable.h"
#include "SE_IO.h"
#include <stdlib.h>
#include <string.h>
#include <utility>
/////////////////////////////////
static const std::string ELEMENT_NODE = "Element";
//////////////////////////////////

/////////////////////////////////
SE_ElementManager::SE_ElementManager()
{
    mRoot = NULL;
}
SE_ElementManager::~SE_ElementManager()
{
    if(mRoot)
        delete mRoot;
}

void SE_ElementManager::load(const char* elementURI)
{
	if(mRoot)
		delete mRoot;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mRoot = resourceManager->getElement(elementURI);
}

SE_Spatial* SE_ElementManager::createSpatial()
{
    if(!mRoot)
        return NULL;
	mRoot->spawn();
	mRoot->update(0);
	SE_Spatial* spatial = mRoot->createSpatial();
    spatial->setLocalTranslate(SE_Vector3f(0, 0, 0));
    spatial->setLocalScale(SE_Vector3f(1, 1, 1));
    SE_Vector4f c1(1, 0, 0, 0);
    SE_Vector4f c2(0, -1, 0, 0);
    SE_Vector4f c3(0, 0, 1, 0);
    SE_Vector4f c4(-mRoot->getWidth() / 2, mRoot->getHeight() / 2, 0, 1);
    SE_Matrix4f localM;
    localM.setColumn(0, c1);
    localM.setColumn(1, c2);
    localM.setColumn(2, c3);
    localM.setColumn(3, c4);
    spatial->setPostMatrix(localM);
    return spatial;
}
class SE_FindByName : public SE_ElementTravel
{
public:
    SE_FindByName()
    {
        selectedElement = NULL;
    }
    void visit(SE_Element* e)
    {
		if(elementID == e->getID())
            selectedElement = e;
    }
    SE_Element* selectedElement;
    SE_ElementID elementID;
};
SE_Element* SE_ElementManager::findByID(const SE_ElementID& id)
{
    SE_FindByName fbn;
    fbn.elementID = id;
    if(mRoot)
        mRoot->travel(&fbn);
    return fbn.selectedElement;
}
void SE_ElementManager::addElement(SE_Element* parent, SE_Element* child)
{}
void SE_ElementManager::removeElement(SE_Element* e)
{}
