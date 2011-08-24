// dllmain.cpp : Defines the entry point for the DLL application.
#include "stdafx.h"
#include <3dsmaxport.h>
#include <max.h>
#include <plugapi.h> 
#include "SEMaxExporter.h"
HMODULE gModule;
#define SE_EXPORTER_ID Class_ID(0x371660db, 0x10f1660)
BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	    gModule = hModule;
		DisableThreadLibraryCalls(hModule);
		break;
	}
	return TRUE;
}
__declspec( dllexport ) const TCHAR* LibDescription() 
{
	return _T("This is exporter for SE engine");
}

/// MUST CHANGE THIS NUMBER WHEN ADD NEW CLASS 
__declspec( dllexport ) int LibNumberClasses() 
{
	return 1;
}

ClassDesc* GetSEExpDesc();
__declspec( dllexport ) ClassDesc* LibClassDesc(int i) 
{
	switch(i) {
	case 0: return GetSEExpDesc();
	default: return 0;
	}
}

__declspec( dllexport ) ULONG LibVersion() 
{
	return Get3DSMAXVersion();
}

// Let the plug-in register itself for deferred loading
__declspec( dllexport ) ULONG CanAutoDefer()
{
	return 1;
}

class SEExpClassDesc: public ClassDesc {
public:
	int				IsPublic() { return 1; }
	void*			Create(BOOL loading = FALSE) { return new SEMaxExporter; } 
	const TCHAR*	ClassName() { return _T("SEMaxExporter"); }
	SClass_ID		SuperClassID() { return SCENE_EXPORT_CLASS_ID; } 
	Class_ID		ClassID() { return SE_EXPORTER_ID; }
	const TCHAR*	Category() { return _T("Standard"); }
};

static SEExpClassDesc* seExpDesc = NULL;

ClassDesc* GetSEExpDesc()
{
	if(seExpDesc == NULL)
	{
		seExpDesc = new SEExpClassDesc;
	}
	return seExpDesc;
}


