#include <string.h>
#ifdef _WIN32
  #include <windows.h>
#else
  #include <unistd.h>
#endif
#include "base2.h"
#include "pipe.h"

inline void ParsePath(char *szDir, char *szFile, const char *szPath) {
  char *lpSeperator;
  strcpy(szDir, szPath);
  lpSeperator = strrchr(szDir, PATH_SEPERATOR);
  if (lpSeperator == NULL) {
    szDir[0] = '\0';
    strcpy(szFile, szPath);
  } else {
    *lpSeperator = '\0';
    strcpy(szFile, lpSeperator + 1);
  }
}

#ifdef _WIN32

#ifdef PIPE_DLL

extern "C" __declspec(dllexport) VOID WINAPI PipeOpen(PipeStruct *lppipe, LPCSTR szProcFile);
extern "C" __declspec(dllexport) VOID WINAPI PipeClose(PipeStruct *lppipe);
extern "C" __declspec(dllexport) LPSTR WINAPI PipeLineInput(PipeStruct *lppipe);
extern "C" __declspec(dllexport) VOID WINAPI PipeLineOutput(PipeStruct *lppipe, LPCSTR szLineStr);

VOID WINAPI PipeOpen(PipeStruct *lppipe, LPCSTR szProcFile) {
  lppipe->Open(szProcFile);
}

VOID WINAPI PipeClose(PipeStruct *lppipe) {
  lppipe->Close();
}

LPSTR WINAPI PipeLineInput(PipeStruct *lppipe) {
  static char szBuffer[LINE_INPUT_MAX_CHAR];
  if (lppipe->LineInput(szBuffer)) {
    return szBuffer;
  } else {
    return NULL;
  }
}

VOID WINAPI PipeLineOutput(PipeStruct *lppipe, LPCSTR szLineStr) {
  lppipe->LineOutput(szLineStr);
}

#endif

void PipeStruct::Open(const char *szProcFile) {
  DWORD dwMode;
  HANDLE hStdinRead, hStdinWrite, hStdoutRead, hStdoutWrite;
  SECURITY_ATTRIBUTES sa;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  TCHAR szFileDir[PATH_MAX_CHAR], szFileName[PATH_MAX_CHAR], szCurDir[PATH_MAX_CHAR];
  TCHAR szProcFileWide[1024];
	memset(fileWideChar, 0, sizeof(wchar_t) * 512);
	MultiByteToWideChar(CP_ACP, 0, filePath, -1, fileWideChar, 511);
  if (szProcFile == NULL) {
    hInput = GetStdHandle(STD_INPUT_HANDLE);
    hOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    bConsole = GetConsoleMode(hInput, &dwMode);
  } else {
    GetCurrentDirectory(PATH_MAX_CHAR, szCurDir);
    ParsePath(szFileDir, szFileName, szProcFile);
    SetCurrentDirectory(szFileDir);

    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;
    CreatePipe(&hStdinRead, &hStdinWrite, &sa, 0);
    CreatePipe(&hStdoutRead, &hStdoutWrite, &sa, 0);
    si.cb = sizeof(STARTUPINFO);
    si.lpReserved = si.lpDesktop = si.lpTitle = NULL;
    si.dwFlags = STARTF_USESTDHANDLES;
    si.cbReserved2 = 0;
    si.lpReserved2 = NULL;
    si.hStdInput = hStdinRead;
    si.hStdOutput = hStdoutWrite;
    si.hStdError = hStdoutWrite;
    CreateProcess(NULL, (LPSTR) szFileName, NULL, NULL, TRUE, DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP | IDLE_PRIORITY_CLASS, NULL, NULL, &si, &pi);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    CloseHandle(hStdinRead);
    CloseHandle(hStdoutWrite);
    hInput = hStdoutRead;
    hOutput = hStdinWrite;
    bConsole = FALSE;

    SetCurrentDirectory(szCurDir);
  }
  if (bConsole) {
    SetConsoleMode(hInput, dwMode & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
    FlushConsoleInputBuffer(hInput);
  }
  nBytesLeft = 0;
  nReadEnd = 0;
}

void PipeStruct::Close(void) const {
  CloseHandle(hInput);
  CloseHandle(hOutput);
}

void PipeStruct::ReadInput(void) {
  DWORD dwBytes;
  ReadFile(hInput, szBuffer + nReadEnd, LINE_INPUT_MAX_CHAR - nReadEnd, &dwBytes, NULL);
  nReadEnd += dwBytes;
  if (nBytesLeft > 0) {
    nBytesLeft -= dwBytes;
  }
}

bool PipeStruct::CheckInput(void) {
  DWORD dwEvents, dwBytes;
  if (bConsole) { // a tty, or an un-redirected handle
    GetNumberOfConsoleInputEvents(hInput, &dwEvents);
    return dwEvents > 1;
  } else if (nBytesLeft > 0) { // a pipe with remainder data
    return true;
  } else if (PeekNamedPipe(hInput, NULL, 0, NULL, &dwBytes, NULL)) { // a pipe without remainder data 
    nBytesLeft = dwBytes;
    return nBytesLeft > 0;
  } else { // a file, always true
    return true;
  }
}

void PipeStruct::LineOutput(const char *szLineStr) const {
  DWORD dwBytes;
  int nStrLen;
  char szWriteBuffer[LINE_INPUT_MAX_CHAR];
  nStrLen = strlen(szLineStr);
  memcpy(szWriteBuffer, szLineStr, nStrLen);
  szWriteBuffer[nStrLen] = '\r';
  szWriteBuffer[nStrLen + 1] = '\n';
  WriteFile(hOutput, szWriteBuffer, nStrLen + 2, &dwBytes, NULL);
}

#else

#include <stdlib.h>
#include <signal.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>

void PipeStruct::Open(const char *szProcFile) {
  int nStdinPipe[2], nStdoutPipe[2];
  char szFileDir[PATH_MAX_CHAR], szFileName[PATH_MAX_CHAR];
  if (szProcFile == NULL) {
    nInput = STDIN_FILENO;
    nOutput = STDOUT_FILENO;
  } else {
    pipe(nStdinPipe);
    pipe(nStdoutPipe);
    if (fork() == 0) {
      close(nStdinPipe[1]);
      close(nStdoutPipe[0]);
      dup2(nStdinPipe[0], STDIN_FILENO);
      close(nStdinPipe[0]);
      dup2(nStdoutPipe[1], STDOUT_FILENO);
      dup2(nStdoutPipe[1], STDERR_FILENO);
      close(nStdoutPipe[1]);

      ParsePath(szFileDir, szFileName, szProcFile);
      chdir(szFileDir);

      nice(20);
      execl(szFileName, szFileName, NULL);
      exit(EXIT_FAILURE);
    }
    close(nStdinPipe[0]);
    close(nStdoutPipe[1]);
    nInput = nStdoutPipe[0];
    nOutput = nStdinPipe[1];
  }
  nReadEnd = 0;
}

void PipeStruct::Close(void) const {
  close(nInput);
  close(nOutput);
}

void PipeStruct::ReadInput(void) {
  nReadEnd += read(nInput, szBuffer + nReadEnd, LINE_INPUT_MAX_CHAR - nReadEnd);
}

bool PipeStruct::CheckInput(void) {
  fd_set set;
  timeval tv;
  int val;
  FD_ZERO(&set);
  FD_SET(nInput, &set);
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  val = select(nInput + 1, &set, NULL, NULL, &tv);
  return (val > 0 && FD_ISSET(nInput, &set) > 0);
}

void PipeStruct::LineOutput(const char *szLineStr) const {
  int nStrLen;
  char szWriteBuffer[LINE_INPUT_MAX_CHAR];
  nStrLen = strlen(szLineStr);
  memcpy(szWriteBuffer, szLineStr, nStrLen);
  szWriteBuffer[nStrLen] = '\n';
  write(nOutput, szWriteBuffer, nStrLen + 1);
}

#endif

bool PipeStruct::GetBuffer(char *szLineStr) {
  char *lpFeedEnd;
  int nFeedEnd;
  lpFeedEnd = (char *) memchr(szBuffer, '\n', nReadEnd);
  if (lpFeedEnd == NULL) {
    return false;
  } else {
    nFeedEnd = lpFeedEnd - szBuffer;
    memcpy(szLineStr, szBuffer, nFeedEnd);
    szLineStr[nFeedEnd] = '\0';
    nFeedEnd ++;
    nReadEnd -= nFeedEnd;
    memcpy(szBuffer, szBuffer + nFeedEnd, nReadEnd);
    lpFeedEnd = (char *) strchr(szLineStr, '\r');
    if (lpFeedEnd != NULL) {
      *lpFeedEnd = '\0';
    }
    return true;
  }
}

bool PipeStruct::LineInput(char *szLineStr) {
  if (GetBuffer(szLineStr)) {
    return true;
  } else if (CheckInput()) {
    ReadInput();
    if (GetBuffer(szLineStr)) {
      return true;
    } else if (nReadEnd == LINE_INPUT_MAX_CHAR) {
      memcpy(szLineStr, szBuffer, LINE_INPUT_MAX_CHAR - 1);
      szLineStr[LINE_INPUT_MAX_CHAR - 1] = '\0';
      szBuffer[0] = szBuffer[LINE_INPUT_MAX_CHAR - 1];
      nReadEnd = 1;
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}
