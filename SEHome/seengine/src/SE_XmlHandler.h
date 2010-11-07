#ifndef SE_XMLHANDLER_H
#define SE_XMLHANDLER_H

/////////////////////////////////
template <typename T, typename PROPERTY>
class SE_XmlElementHandler
{
public:
    virtual ~SE_XmlElementHandler() {}
	SE_XmlElementHandler(PROPERTY* p)
    {
        pro = p;
    }
    virtual void handle(T* parent, TiXmlElement* xmlElement, unsigned int indent)
	{}
    PROPERTY* pro;
};
template <typename T, typename PROPERTY>
class SE_XmlElementHandlerManager
{
public:
	SE_XmlElementHandlerManager(PROPERTY* p) : pro(p)
	{}
    SE_XmlElementHandler<T, PROPERTY>* getHandler(const char* name)
    {
        return NULL;
    }
    PROPERTY* pro;
    //std::map<std::string, SE_XmlElementHandler<T>* > mElementHandlerManager;
};
template <typename PARENTT, typename PROPERTY>
class SE_XmlElementCalculus
{
public:
	SE_XmlElementCalculus(PROPERTY* p) : mXmlElementHandlerManager(p)
	{}
    void handleText(PARENTT* parent, TiXmlText* text)
    {}
    void handleDeclaration(TiXmlDeclaration* decl)
    {}
	void handleElement(PARENTT* parent, const char* elementName, TiXmlElement* pElement, unsigned int indent);
    void handleXmlChild(PARENTT* parent, TiXmlNode* currNode, unsigned int indent);
private:
   SE_XmlElementHandlerManager<PARENTT, PROPERTY> mXmlElementHandlerManager;
};
template <typename PARENTT, typename PROPERTY>
void SE_XmlElementCalculus<PARENTT, PROPERTY>::handleElement(PARENTT* parent, const char* elementName, TiXmlElement* pElement, unsigned int indent)
{
    if(!pElement)
        return;
    SE_XmlElementHandler<PARENTT, PROPERTY>* pXmlElementHandler = mXmlElementHandlerManager.getHandler(elementName);
    if(!pXmlElementHandler)
        return;
    pXmlElementHandler->handle(parent, pElement, indent);
}
template <typename PARENTT, typename PROPERTY>
void SE_XmlElementCalculus<PARENTT, PROPERTY>::handleXmlChild(PARENTT* parent, TiXmlNode* currNode, unsigned int indent)
{
    if(!currNode)
        return;
    TiXmlNode* pChild;
    TiXmlText* pText;
    int t = currNode->Type();
    int num = 0;
    switch(t)
    {
    case TiXmlNode::TINYXML_DOCUMENT:
        LOGI("...Document\n");
		for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
        {
            handleXmlChild(parent, pChild, indent + 1);
        }
        break;
    case TiXmlNode::TINYXML_ELEMENT:
        LOGI("...Element[%s]\n", currNode->Value());
        handleElement(parent, currNode->Value(), currNode->ToElement(), indent + 1);
        break;
    case TiXmlNode::TINYXML_COMMENT:
        LOGI("...Comment:[%s]\n", currNode->Value());
        break;
    case TiXmlNode::TINYXML_TEXT:
        pText = currNode->ToText();
        LOGI("...Text: [%s]\n", pText->Value());
        handleText(parent, pText);
        break;
    case TiXmlNode::TINYXML_DECLARATION:
        LOGI("...Declaration\n");
        handleDeclaration(currNode->ToDeclaration());
        break;
    }
}
#endif