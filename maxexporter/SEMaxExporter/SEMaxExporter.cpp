// SEMaxExporter.cpp : Defines the exported functions for the DLL application.
//
#include "SEMaxExporter.h"
#include <stdio.h>
SEMaxExporter::SEMaxExporter()
{}
SEMaxExporter::~SEMaxExporter()
{}
int    SEMaxExporter::ExtCount()
{
	return 1;
}
const TCHAR * SEMaxExporter::Ext(int n)
{
	switch(n) {
	case 0:
		// This cause a static string buffer overwrite
		// return GetString(IDS_EXTENSION1);
		return _T("CBF");
	}
	return _T("");
}
const TCHAR * SEMaxExporter::LongDesc()     // Long ASCII description (i.e. "Ascii Export") 
{
	return _T("This is exporter for SE cbf file");
}
const TCHAR * SEMaxExporter::ShortDesc()    // Short ASCII description (i.e. "Ascii")
{
	return _T("cbf file");
}
const TCHAR * SEMaxExporter::AuthorName()    // ASCII Author name
{
	return _T("luwei");
}
const TCHAR * SEMaxExporter::CopyrightMessage()   // ASCII Copyright message 
{
	return _T("copyright");
}
const TCHAR * SEMaxExporter::OtherMessage1()   // Other message #1
{
	return _T("m1");
}
const TCHAR * SEMaxExporter::OtherMessage2()   // Other message #2
{
	return _T("m2");
}
unsigned int SEMaxExporter::Version()     // Version number * 100 (i.e. v3.01 = 301) 
{
	return 300;
}
void	SEMaxExporter::ShowAbout(HWND hWnd)  // Show DLL's "About..." box
{

}
int	SEMaxExporter::DoExport(const TCHAR *name,ExpInterface *ei,Interface *i, BOOL suppressPrompts, DWORD options) // Export	file
{
	FILE* pStream = _tfopen(name,_T("wt"));
	fprintf(pStream, "My Exporter Hello\n");

	fclose(pStream);
	return 1;
}
BOOL	SEMaxExporter::SupportsOptions(int ext, DWORD options)
{
	if(ext != 0)	// We only support one extension
		return FALSE;
	return(options == SCENE_EXPORT_SELECTED) ? TRUE : FALSE;
}