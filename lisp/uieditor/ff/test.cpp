#include <windows.h>
#include <stdio.h>
STARTUPINFO si;
PROCESS_INFORMATION pi;
extern "C" __declspec(dllexport) int StartUI(int c)
{
    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );
    char* name = "E:\\sourcecode\\Imagination Technologies\\POWERVR SDK\\OGLES2_WINDOWS_PCEMULATION_2.06.26.0649\\TrainingCourse\\SEHome\\OGLES2\\Build\\WindowsPC\\Debug\\OGLES2IntroducingPVRShell.exe";
	char* path = "E:\\sourcecode\\Imagination Technologies\\POWERVR SDK\\OGLES2_WINDOWS_PCEMULATION_2.06.26.0649\\TrainingCourse\\SEHome\\OGLES2\\Build\\WindowsPC\\Debug\\OGLES2IntroducingPVRShell.exe -width=480 -height=800" ;
    // Start the child process. 
    if( !CreateProcess( NULL,   // No module name (use command line)
        path,        // Command line
        NULL,           // Process handle not inheritable
        NULL,           // Thread handle not inheritable
        FALSE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        NULL,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi )           // Pointer to PROCESS_INFORMATION structure
    ) 
    {
        printf( "CreateProcess failed (%d).\n", GetLastError() );
        return 0;
    }
    return 1;
}
