#include "SE_ElementManager.h"
    SE_ElementManager();
    ~SE_ElementManager();
    void load(const wchar_t* filePath);
    SE_Spatial* createSpatial();
    void addElement(SE_Element* parent, SE_Element* child);
    void removeElement(SE_Element* e);
    SE_Element* getRoot();
