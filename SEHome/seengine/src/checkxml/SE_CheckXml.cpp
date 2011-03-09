#include "SE_CheckXml.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <list>
#include <stack>
#include "tinyxml.h"
#include "SE_ObjectManager.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ElementSchema.h"
#include "SE_Utils.h"
#include "SE_Sequence.h"
#include "SE_ColorEffectController.h"
#include "SE_ParamManager.h"
#include "SE_ImageMap.h"
#include "SE_ObjectManager.h"
#include "SE_TableManager.h"
#include "SE_ElementContent.h"
#include "SE_Log.h"
const int BUF_SIZE = 1000;
const char* xmlElementKeyword[] = {"Image", "Action", "StateTable", "Element", "MountPoint", "SequenceFrame",
                                 "Frame", "AnimationObject", "Property", "StateChange", "Trigger", "Param", "Struct",
                                 "ImageTable", "ImageItem", "MarkR", "MarkG", "MarkB", "MarkA", "ColorEffectFrameController"};
const char* elementKeyword[] = {"id", "x", "y", "pivotx", "pivoty", "mountpointref", "layer", 
                               "dataref"};
/*
const char* elementContent[] = {};
const char* elementContentImageAttrib[] = {"dataref"};
const char* elementContentActionAttrib[] = {"dataref"};
const char* elementContentStateTableAttrib[] = {"dataref"};
*/
const char* imageTableKeyword[] = {"id", "pivotx", "pivoty", "name", "startx", "starty", "endx", "endy", "mirror"};
//const char* imageItemAttrib[] = {"id"};
//const char* imageImageAttrib[] = {"id", "pivotx", "pivoty"};
const char* actionKeyword[] = {"id", "pivotx", "pivoty", "x", "y",  
                               "key", "sequenceref", "coloreffectref","layer", "mountpointref",
							   "rotation", "scale"};
//const char* mountpointKeyword[] = {"id", "x", "y"};
const char* sequenceKeyword[] = {"id", "pivotx", "pivoty", "x", "y", "key", "imageref", "mountpointref", "rotation"};
const char* colorEffectControllerKeyword[] = {"id", "x", "y", "pivotx", "pivoty", "key", 
                                   "background", "channel", "alpha", 
								   "pivotx", "pivoty", "mountpointref", 
								   "texture", "fn", "alpha", "texturefn", "color"
                                  };
const char* stateTableKeyword[] = {"id", "pivotx", "pivoty", "Property", "statechange", "actionref", "imageref"};
/*
const char* colorEffectFrameAttrib[] = {};
const char* colorEffectFramMarkAttrib[] = {};
*/
static FILE* outputFile = NULL;
static void outputString(const char* buf, int size)
{
	if(outputFile)
	{
        fprintf(outputFile, "%s\n", buf);
	}
}
static void OUTSTRING(const char* fmt, ...)
{
    va_list ap;
    char buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
	outputString(buf, strlen(buf));
}
class _Keyword
{
public:
	_Keyword(const char** keywordarray, int size)
	{
		mKeyword = keywordarray;
		mSize = size;
	}
	bool find(const char* name)
	{
		for(int i = 0 ; i < mSize; i++)
		{
			if(!strcmp(mKeyword[i], name))
			{
				return true;
			}
		}
		return false;
	}
protected:
	const char** mKeyword;
	int mSize;
};
class ElementKeyword : public _Keyword
{
public:
	ElementKeyword(const char** keyword, int size) : _Keyword(keyword, size)
	{}
	bool find(const char* keyword)
	{
        return false;
	}

};
class ElementContentKeyword
{};
class ElementContentImageAttribKeyword
{};
class ElementContentActionAttribKeyword
{};
class ImageTableAttribKeyword
{};
class ImageItemAttribKeyword
{};
class ImageImageAttribKeyword
{};
class ActionAttribKeyword
{};
class ColorEffectAttribKeyword
{};
class ColorEffectFrameAttribKeyword
{};
class ColorEffectFrameMarkAttribKeyword
{};
static int getFileLen(FILE* fp)
{
	int		pos;
	int		end;

	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);

	return end;

}
#if defined(WIN32)
#define chDIMOF(Array) (sizeof(Array) / sizeof(Array[0]))
struct _FileNameData
{
	TCHAR buf[BUF_SIZE];
};
std::list<_FileNameData> fileNameList;
typedef struct {
	int nDepth;
	BOOL fRecurse;
	TCHAR szBuf[BUF_SIZE];
	int nIndent;
	BOOL fOk;
	BOOL fIsDir;
	WIN32_FIND_DATA FindData;
} DIRWALKDATA, *LPDIRWALKDATA;
static BOOL IsChildDir (WIN32_FIND_DATA *lpFindData) {

   return(
      ((lpFindData->dwFileAttributes & 
         FILE_ATTRIBUTE_DIRECTORY) != 0) &&
      (lstrcmp(lpFindData->cFileName, __TEXT("."))  != 0) &&
      (lstrcmp(lpFindData->cFileName, __TEXT("..")) != 0));
}


/////////////////////////////////////////////////////////////
static BOOL FindNextChildDir (HANDLE hFindFile,
   WIN32_FIND_DATA *lpFindData) {

   BOOL fFound = FALSE;

   do {
      fFound = FindNextFile(hFindFile, lpFindData);
   } while (fFound && !IsChildDir(lpFindData));

   return(fFound);
}


/////////////////////////////////////////////////////////////


static HANDLE FindFirstChildDir (LPTSTR szPath,
   WIN32_FIND_DATA *lpFindData) {

   BOOL fFound;
   HANDLE hFindFile = FindFirstFile(szPath, lpFindData);

   if (hFindFile != INVALID_HANDLE_VALUE) {
      fFound = IsChildDir(lpFindData);

      if (!fFound)
         fFound = FindNextChildDir(hFindFile, lpFindData);

      if (!fFound) {
         FindClose(hFindFile);
         hFindFile = INVALID_HANDLE_VALUE;
      }
   }
   return(hFindFile);
}


/////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////


// Walk the directory structure and fill a list box with
// filenames. If pDW->fRecurse is set, list any child
// directories by recursively calling DirWalkRecurse.
static void DirWalkRecurse (LPDIRWALKDATA pDW) {
   HANDLE hFind;

   pDW->nDepth++;

   pDW->nIndent = 3 * pDW->nDepth;
   _stprintf(pDW->szBuf, __TEXT("%*s"), pDW->nIndent,
      __TEXT(""));

   GetCurrentDirectory(chDIMOF(pDW->szBuf) - pDW->nIndent,
      &pDW->szBuf[pDW->nIndent]);
   //store data in filenamelist
   /*
   _FileNameData fnd;
   memcpy(fnd.buf, pDW->szBuf, BUF_SIZE);
   fileNameList.push_back(fnd);
   */
   //end
   hFind = FindFirstFile(__TEXT("*.*"), &pDW->FindData);
   pDW->fOk = (hFind != INVALID_HANDLE_VALUE);

   while (pDW->fOk) {
      pDW->fIsDir = pDW->FindData.dwFileAttributes &
         FILE_ATTRIBUTE_DIRECTORY;
      if (!pDW->fIsDir ||
         (!pDW->fRecurse && IsChildDir(&pDW->FindData))) {

         _stprintf(pDW->szBuf,
            pDW->fIsDir ? __TEXT("%*s[%s]") : __TEXT("%*s%s"),
            pDW->nIndent, __TEXT(""),
            pDW->FindData.cFileName);

         //ListBox_AddString(pDW->hwndTreeLB, pDW->szBuf);
           _FileNameData fnd;
          memcpy(fnd.buf, pDW->szBuf, BUF_SIZE);
          fileNameList.push_back(fnd);
      }
      pDW->fOk = FindNextFile(hFind, &pDW->FindData);
   }
   if (hFind != INVALID_HANDLE_VALUE)
      FindClose(hFind);

   if (pDW->fRecurse) {
      // Get the first child directory.
      hFind = FindFirstChildDir(
         __TEXT("*.*"), &pDW->FindData);
      pDW->fOk = (hFind != INVALID_HANDLE_VALUE);
      while (pDW->fOk) {
         // Change into the child directory.
         if (SetCurrentDirectory(pDW->FindData.cFileName)) {

            // Perform the recursive walk into the child
            // directory.  Remember that some members of pDW 
            // will be overwritten by this call.
            DirWalkRecurse(pDW);

            // Change back to the child's parent directory.
            SetCurrentDirectory(__TEXT(".."));
         }

         pDW->fOk = FindNextChildDir(hFind, &pDW->FindData);
      }

      if (hFind != INVALID_HANDLE_VALUE)
         FindClose(hFind);
   }
   pDW->nDepth--;
}


/////////////////////////////////////////////////////////////


// Walk the directory structure and fill a list box with
// filenames. This function sets up a call to 
// DirWalkRecurse, which does the real work.

static void DirWalk (LPCTSTR pszRootPath,  BOOL fRecurse) {      // Expand subdirectories.

   TCHAR szCurrDir[_MAX_DIR];
   DIRWALKDATA DW;

   // Save the current directory so that it can 
   // be restored later.
   GetCurrentDirectory(chDIMOF(szCurrDir), szCurrDir);

   // Set the current directory to where we want
   // to start walking.
   SetCurrentDirectory(pszRootPath);

   // nDepth is used to control indenting. The value -1 will
   // cause the first level to display flush left.
   DW.nDepth = -1;
   DW.fRecurse = fRecurse;

   // Call the recursive function to walk the subdirectories.
   DirWalkRecurse(&DW);

   // Restore the current directory to what it was 
   // before the function was called.
   SetCurrentDirectory(szCurrDir);
}

#else
#endif

class xmlVisitorImpl : public TiXmlVisitor
{
public:
	enum {ENTER_MODE, EXIT_MODE};
	xmlVisitorImpl(const char** keyword, int size) : keywordData(keyword, size), xmlElementKeywordData(xmlElementKeyword, sizeof(xmlElementKeyword) / sizeof(const char*))
	{
		//mState = EXIT_MODE;
		//mDepth = 0;
	}
	virtual ~xmlVisitorImpl() {}

	/// Visit a document.
	virtual bool VisitEnter( const TiXmlDocument& doc )
	{ 
		//mDepth++;
		std::string str = doc.Value();
		mStack.push(str);
		return true; 
	}
	/// Visit a document.
	virtual bool VisitExit( const TiXmlDocument& doc )			
	{ 
		std::string str = doc.Value();
		if(str != mStack.top())
		{
			OUTSTRING("... %s xml file has not document end\n");
		}
		mStack.pop();
		return true; 
	}

	/// Visit an element.
	virtual bool VisitEnter( const TiXmlElement& element, const TiXmlAttribute* firstAttribute )
	{ 
		std::string str = element.Value();
		bool xmlElementFound = xmlElementKeywordData.find(str.c_str());
		if(!xmlElementFound)
		{
			OUTSTRING(".. %s xml element is not defined ", str.c_str());
		}

		mStack.push(str);
		const TiXmlAttribute* pAttribute = firstAttribute;
		while(pAttribute)
		{
			const char* name = pAttribute->Name();
		    std::string strvalue = SE_Util::trim(pAttribute->Value());
		    const char* value = strvalue.c_str();
			bool found = keywordData.find(name);
			if(!found)
			{
				OUTSTRING("... [%s]/[%s]: attribute keyword [%s] define error", fileName.c_str(), element.Value(), name);
			}
			pAttribute = pAttribute->Next();
		}
		return true; 
	}
	/// Visit an element.
	virtual bool VisitExit( const TiXmlElement& element )		
	{
		std::string str = element.Value();
		std::string top = mStack.top();
		if(str != top)
		{
			OUTSTRING("... [%s]/[%s]: before element has not exit : %d\n", fileName.c_str(), element.Value(), element.Row());
		}
        mStack.pop();
		return true; 
	}

	/// Visit a declaration
	virtual bool Visit( const TiXmlDeclaration& declaration )	
	{ 
		return true; 
	}
	/// Visit a text node
	virtual bool Visit( const TiXmlText& text )					
	{ 
		return true; 
	}
	/// Visit a comment node
	virtual bool Visit( const TiXmlComment& comment )			
	{ 
		return true; 
	}
	/// Visit an unknow node
	virtual bool Visit( const TiXmlUnknown& unknown )			
	{ 
		OUTSTRING("... has a unkown xml element : %s \n", unknown.Value());
		return true; 
	}
    _Keyword keywordData;
	std::stack<std::string> mStack;
	std::string fileName;
	_Keyword xmlElementKeywordData;
};
static TiXmlDocument* getDocument(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	std::string filePath = resourceManager->getLayoutPath() + SE_SEP + fileName;
	TiXmlDocument* doc = new TiXmlDocument(filePath.c_str());
    doc->LoadFile();
    if(doc->Error() && doc->ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		OUTSTRING("can not open xml file: %s\n", filePath);
        return NULL;
    }
	return doc;
}
static void checkResolvedURI(const std::string& uri)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(uri.c_str(), "/");
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	switch(t)
	{
	case SE_ELEMENT_TABLE:
		{
			SE_ElementSchema* e = resourceManager->getElementSchema(uri.c_str());
			if(!e)
			{
				OUTSTRING("... %s is not define in %s", uri.c_str(), strList[0].c_str());
			}
		}
		break;
	case SE_IMAGE_TABLE:
		{
			SE_ImageUnit iu = resourceManager->getImageUnit(uri.c_str());
			if(!iu.isValid())
			{
				OUTSTRING("... %s is not defined in %s", uri.c_str(), strList[0].c_str());
			}
		}
		break;
	case SE_ACTION_TABLE:
		{
			SE_Action* a = resourceManager->getAction(uri.c_str());
			if(!a)
			{
				OUTSTRING("... %s is not defined in %s", uri.c_str(), strList[0].c_str());
			}
		}
		break;
	case SE_SEQUENCE_TABLE:
		{
			SE_Sequence* s = resourceManager->getSequence(uri.c_str());
			if(!s)
			{
				OUTSTRING("... %s is not defined in %s", uri.c_str(), strList[0].c_str());
			}
		}
		break;
	case SE_COLOREFFECT_TABLE:
		{
			SE_ColorEffectController* c = resourceManager->getColorEffectController(uri.c_str());
			if(!c)
			{
				OUTSTRING("... %s is not defined in %s", uri.c_str(), strList[0].c_str());
			}
		}
		break;
	}
}
static void checkURI(const char* strURI)
{
	SE_Util::SplitStringList paramStr = SE_Util::extractParamString(strURI);
	SE_Util::SplitStringList::iterator it;
	bool paramOK = true;
	for(it = paramStr.begin() ; it != paramStr.end() ; it++)
	{
	    SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
		bool ok = false;
		std::string v = paramManager->getString(it->c_str(), ok);
		if(!ok)
		{
			OUTSTRING(".. param error: %s is not defined ", it->c_str());
			paramOK = false;
		}
	}
	if(paramOK)
	{
		std::string resolvedURI = SE_Util::resolveParamString(strURI);
		checkResolvedURI(resolvedURI);
	}
}
class ElementTravel : public SE_ElementSchemaVisitor
{
public:
	void visit(SE_ElementSchema* e)
	{
		int contentNum = e->getContentNum();
		for(int i = 0 ; i < contentNum ; i++)
		{
			SE_ElementContent* ec = e->getContent(i);
			SE_StringID strURI = ec->getURI();
			checkURI(strURI.getStr());
		}
	}
};
class ElementVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ElementSchema*>
{
public:
	void visit(const SE_StringID& id, const SE_ElementSchema* element)
	{
		ElementTravel et;
		((SE_ElementSchema*)element)->travel(&et);
	}
};
class ElementMapVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ElementSchemaMap*>
{
public:
	void visit(const SE_StringID& id, const SE_ElementSchemaMap* elementMap)
	{
		if(id == elementMapID)
		{
            ElementVisitor v;
            elementMap->traverse(v);
		}
	}
	SE_StringID elementMapID;
};
static void checkDocument(const char* fileName, TiXmlDocument* doc, const char** keyword, int size)
{
    xmlVisitorImpl xvi(keyword, size);
	xvi.fileName = fileName;
	doc->Accept(&xvi);
}
static void checkElementTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadElementSchema(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, elementKeyword, sizeof(elementKeyword) / sizeof(const char*));
	    delete doc;
	}
	const SE_ElementSchemaTable& elementTable = resourceManager->getElementSchemaTable();
	ElementMapVisitor emv;
	emv.elementMapID = fileName;
	elementTable.traverse(emv);
}
class ImageItemVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageItem*>
{
public:
	void visit(const SE_StringID& id, const SE_ImageItem* imageItem)
	{
		const SE_ImageItemProperty& imageItemProperty = imageItem->getProperty();
		SE_ImageDataID imageDataID = imageItemProperty.getImageDataID();
		SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		SE_ImageData* imageData = resourceManager->loadImage(imageDataID.getStr());
		if(!imageData)
		{
			OUTSTRING("... image file can not find : %s", imageDataID.getStr());
		}
	}
	SE_StringID imageMapSetID;
	SE_StringID imageMapID;
};
class ImageMapVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageMap*>
{
public:
    void visit(const SE_StringID& id, const SE_ImageMap* imageMap)
	{
        ImageItemVisitor iiv;
		iiv.imageMapSetID = this->imageMapSetID;
		iiv.imageMapID = id;
		imageMap->traverse(iiv);
	}
	SE_StringID imageMapSetID;
};

class ImageMapSetVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageMapSet*>
{
public:
	ImageMapSetVisitor()
	{
		findMapSet = false;
	}
	void visit(const SE_StringID& id, const SE_ImageMapSet* imageMapSet)
	{
		if(id == this->imageMapSetID)
		{
            ImageMapVisitor imv;
			imv.imageMapSetID = imageMapSetID;
			imageMapSet->traverse(imv);
			findMapSet = true;
		}
	}
	bool findMapSet;
	SE_StringID imageMapSetID;
};
static void checkImageTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadImageTable(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, imageTableKeyword, sizeof(imageTableKeyword) / sizeof(const char*));
	    delete doc;
	}
	const SE_ImageTable& imageTable = resourceManager->getImageTable();
    ImageMapSetVisitor imv;
	imv.imageMapSetID = fileName;
	imageTable.traverse(imv);
	if(!imv.findMapSet)
	{
		OUTSTRING("... %s image table file can not find", fileName);
	}
}
class ActionVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_Action*>
{
public:
	void visit(const SE_StringID& id, const SE_Action* action)
	{
		const std::list<SE_Action::_ActionLayer*>& actionLayerList = action->getActionLayerList();
		std::list<SE_Action::_ActionLayer*>::const_iterator it;
		for(it = actionLayerList.begin() ; it!= actionLayerList.end() ; it++)
		{
			const SE_Action::_ActionLayer* actionLayer = *it;
			const SE_KeyFrameSequence<SE_ActionUnit*>& sequences = actionLayer->sequences;
            std::vector<SE_TimeKey> keys = sequences.getKeys();
			for(size_t i = 0 ; i < keys.size() ; i++)
			{
				SE_KeyFrame<SE_ActionUnit*>* keyframe = sequences.getKeyFrame(keys[i]);
				SE_ActionUnit* data = keyframe->data;
				SE_StringID str = data->getURI();
                checkURI(str.getStr());
			}
		}
	}
};
class ActionSetVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ActionMapSet*>
{
public:
	ActionSetVisitor()
	{
		found = false;
	}
	void visit(const SE_StringID& id, const SE_ActionMapSet* actionMapSet)
	{
        if(id == actionMapID)
		{
			ActionVisitor av;
			actionMapSet->traverse(av);
            found = true;
		}
	}
	SE_StringID actionMapID;
	bool found;
};
static void checkActionTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadAction(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, actionKeyword, sizeof(actionKeyword) / sizeof(const char*));
	    delete doc;
	}
	const SE_ActionTable& actionTable = resourceManager->getActionTable();
    ActionSetVisitor asv;
	asv.actionMapID = fileName;
	actionTable.traverse(asv);
	if(!asv.found)
	{
		OUTSTRING("... %s action table not found", fileName);
	}
}
class ColorEffectVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ColorEffectController*>
{
public:
	void visit(const SE_StringID& id, const SE_ColorEffectController* c)
	{
		std::vector<SE_TimeKey> keys = c->getKeys();
		for(int i = 0 ; i < keys.size() ; i++)
		{
			SE_ColorEffectFrame* cef = c->getKeyFrame(keys[i]);
			SE_StringID background = cef->getBackground();
			checkURI(background.getStr());
			SE_StringID channel = cef->getChannel();
			checkURI(background.getStr());
			for(int i = 0 ; i < 4 ; i++)
			{
				SE_StringID texture = cef->getTexture(i);
				checkURI(texture.getStr());
			}
		}
	}
};
class ColorEffectControllerVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ColorEffectControllerSet*>
{
public:
	void visit(const SE_StringID& id, const SE_ColorEffectControllerSet* cs)
	{
		if(id == colorEffectControllerID)
		{
            ColorEffectVisitor cev;
			cs->traverse(cev);
		}
	}
	SE_StringID colorEffectControllerID;
};
static void checkColorEffectControllerTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadElementSchema(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, colorEffectControllerKeyword, sizeof(colorEffectControllerKeyword) / sizeof(const char*));
	    delete doc;
	}
	const SE_ColorEffectControllerTable& colorEffectControllerTable = resourceManager->getColorEffectControllerTable();
    ColorEffectControllerVisitor cecv;
	cecv.colorEffectControllerID = fileName;
	colorEffectControllerTable.traverse(cecv);
}
class SequenceVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_Sequence*>
{
public:
	void visit(const SE_StringID& id, const SE_Sequence* s)
	{
        std::vector<SE_TimeKey> keys = s->getKeys();
		for(int i = 0 ; i < keys.size() ; i++)
		{
			SE_Sequence::_Frame f = s->getFrame(keys[i]);
			SE_StringID strURI = f.imageref;
			checkURI(strURI.getStr());
		}
	}
};
class SequenceSetVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_SequenceSet*>
{
public:
	void visit(const SE_StringID& id, const SE_SequenceSet* ss)
	{
        if(id == sequenceSetID)
		{
            SequenceVisitor sv;
			ss->traverse(sv);
		}
	}
	SE_StringID sequenceSetID;
};
static void checkSequenceTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadSequence(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, sequenceKeyword, sizeof(sequenceKeyword) / sizeof(const char*));
	    delete doc;
	}
    SequenceSetVisitor ssv;
	ssv.sequenceSetID = fileName;
	const SE_SequenceTable& sequenceTable = resourceManager->getSequenceTable();
	sequenceTable.traverse(ssv);
}
static void checkStateTable(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadElementSchema(fileName);
	TiXmlDocument* doc = getDocument(fileName);
	if(doc)
	{
	    checkDocument(fileName, doc, stateTableKeyword, sizeof(stateTableKeyword) / sizeof(const char*));
	    delete doc;
	}
}
static void handleContent(const char* fileName)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(fileName);
    switch(t)
	{
	case SE_ELEMENT_TABLE:
		checkElementTable(fileName);
		break;
	case SE_IMAGE_TABLE:
		checkImageTable(fileName);
		break;
	case SE_ACTION_TABLE:
		checkActionTable(fileName);
		break;
	case SE_COLOREFFECT_TABLE:
		checkColorEffectControllerTable(fileName);
		break;
	case SE_SEQUENCE_TABLE:
		checkSequenceTable(fileName);
		break;
	case SE_STATE_TABLE:
		break;
	}
}
#if defined(WIN32)
std::string wcharToMultiByte(LPCWSTR wcharStr)
{
    int size = WideCharToMultiByte(CP_OEMCP,NULL, wcharStr,-1,NULL,0,NULL,FALSE);
    char* str = new char[size + 1];
	memset(str, 0, size + 1);
	WideCharToMultiByte(CP_OEMCP,NULL,wcharStr,-1, str,size,NULL,FALSE);
	std::string ret(str);
	delete[] str;
	return ret;
}

void checkXml()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	std::string inputDir = resourceManager->getLayoutPath();
	std::string outputDir = resourceManager->getDataPath();
	std::string outputFilePath = outputDir + SE_SEP + "checkxmlresult.txt";
	FILE* f = fopen(outputFilePath.c_str(), "w");
	if(!f)
	{
		LOGI("can not open output file : %s\n", outputFilePath.c_str());
		return;
	}
	outputFile = f;
    TCHAR inputDirWideChar[512];
	memset(inputDirWideChar, 0, sizeof(TCHAR) * 512);
	MultiByteToWideChar(CP_ACP, 0, inputDir.c_str(), -1, inputDirWideChar, 511);
	DirWalk(inputDirWideChar, false);
	std::string inputStr = inputDir;
	std::list<_FileNameData>::iterator it ;
	for(it = fileNameList.begin() ; it != fileNameList.end() ; it++)
	{
		_FileNameData fnd = *it;
		std::string fileName = wcharToMultiByte(fnd.buf);
		std::string filePath = fileName;
		if(fileName != "")
		{
		    handleContent(filePath.c_str());
		}
	}
	fclose(f);
}
#else
void checkXml(const char* inputDir, const char* outputDir)
{}
#endif
