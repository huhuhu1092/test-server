#include <winsock2.h>
#include <windows.h>
#include <stdio.h>
STARTUPINFO si;
PROCESS_INFORMATION pi;
SECURITY_ATTRIBUTES sa;
extern "C" __declspec(dllexport) int HostToNetShort(int s)
{
	return htons(s);
}
extern "C" __declspec(dllexport) int StartUI(int c)
{
    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );
	ZeroMemory(&sa, sizeof(sa));
	sa.bInheritHandle = true;
	sa.lpSecurityDescriptor = NULL;
	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    char* name = "E:\\sourcecode\\Imagination Technologies\\POWERVR SDK\\OGLES2_WINDOWS_PCEMULATION_2.06.26.0649\\TrainingCourse\\SEHome\\OGLES2\\Build\\WindowsPC\\Debug\\OGLES2IntroducingPVRShell.exe";
	char* cml = "\"E:\\sourcecode\\Imagination Technologies\\POWERVR SDK\\OGLES2_WINDOWS_PCEMULATION_2.06.26.0649\\TrainingCourse\\SEHome\\OGLES2\\Build\\WindowsPC\\Debug\\OGLES2IntroducingPVRShell.exe\" -width=480 -height=800" ;
	char* dir = "E:\\sourcecode\\Imagination Technologies\\POWERVR SDK\\OGLES2_WINDOWS_PCEMULATION_2.06.26.0649\\TrainingCourse\\SEHome\\OGLES2\\Build\\WindowsPC\\Debug";
    // Start the child process. 
    if( !CreateProcess( name,   // No module name (use command line)
        cml,        // Command line
        NULL,//&sa,           // Process handle not inheritable
        NULL,//&sa,           // Thread handle not inheritable
        TRUE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        dir,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi )           // Pointer to PROCESS_INFORMATION structure
    ) 
    {
        printf( "CreateProcess failed (%d).\n", GetLastError() );
        return 0;
    }
    return 1;
}
int main(int argc , char** argv)
{
	StartUI(0);
	return 0;
}