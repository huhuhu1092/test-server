#include "SE_AssetManager.h"
#ifdef ANDROID
#include <stdio.h>
#include "SE_Log.h"
#include <string>
#include <string.h>

SE_AssetManager::SE_AssetManager() {
    mAssetManager = new android::AssetManager();
}

SE_AssetManager::SE_AssetManager(android::AssetManager* am) {
    mAssetManager = am;
}
/*void SE_AssetManager::addAssetPath(const android::String8& path)
{
    void* cookie;
    bool res = mAssetManager->addAssetPath(path, &cookie);
    int success = (res) ? (int)cookie : 0;
#ifdef DEBUG
    if (success == 0)
    {  
        LOGI("## addAssetPath failed !!!### path = %s", path.string());
    } else
    {
        LOGI("## addAssetPath success !!!### path = %s", path.string());
    }
#endif
}

void SE_AssetManager::rmAssetPath(const android::String8& path)
{
    bool res = mAssetManager->rmAssetPath(path);
}*/

android::Asset* SE_AssetManager::openAsset(const char* fileName, int mode)
{
    if (!mAssetManager) {
#ifdef DEBUG
        LOGD("SE_AssertManager : AssertManager is null");
#endif
        return NULL;
    }
    if (mode !=  android::Asset::ACCESS_UNKNOWN && mode !=  android::Asset::ACCESS_RANDOM
        && mode !=  android::Asset::ACCESS_STREAMING && mode !=  android::Asset::ACCESS_BUFFER) {
#ifdef DEBUG
        LOGD("SE_AssertManager : access mode is not legal");
#endif
        return NULL;
    }
    android::Asset* a = mAssetManager->openNonAsset(fileName, ( android::Asset::AccessMode)mode);

    if (a == NULL) {
#ifdef DEBUG
        LOGD("SE_AssertManager : file not found name = %s\n", fileName);
#endif
    }

    return a;
}
void SE_AssetManager::readAsset(const char* fileName, char*& outData, int& outLen)
{
    outData = NULL;
    outLen = 0;
    android::Asset* asset = openAsset(fileName, android::Asset::ACCESS_STREAMING);
    if(!asset) {
        SE_AssetManager::readLargeAsset(fileName, outData, outLen);
        return;
    }
    outLen = asset->getLength();
    outData = new char[outLen];
    if(!outData)
    {
        LOGE("SE_AssertManager : out of memory when read file\n");
        return;
    }
    asset->read(outData, outLen);
    delete asset;    
}

void SE_AssetManager::readLargeAsset(const char* fileName, char*& outData, int& outLen)
{
    std::string fileStrName = fileName;
    //get length begin
    int fileCount = 0;
    while (true) {
        char str[1];
        sprintf(str, "%d", fileCount);
        std::string childFileName = fileStrName + str;
        android::Asset* asset = openAsset(childFileName.c_str(), android::Asset::ACCESS_STREAMING);
        if(!asset) {
            break;
        } else {
            int length = asset->getLength();
            outLen += length;
            delete asset; 
        }
        fileCount ++;
    }
    //get length end
    if (fileCount == 0) {
        return;
    }
    outData = new char[outLen];
    if(!outData)
    {
        LOGE("SE_AssertManager : out of memory when read file\n");
        return;
    }
    char* p = outData;
    for (int count = 0; count < fileCount; count++) {
        char str[1];
        sprintf(str, "%d", count);
        std::string childFileName = fileStrName + str;
        android::Asset* asset = openAsset(childFileName.c_str(), android::Asset::ACCESS_STREAMING);
        if(!asset)
            return;
        int length = asset->getLength();
        if(!p)
        {
            LOGE("SE_AssertManager : out of memory when read file\n");
            return;
        }
        asset->read(p, length);
        p += length;
        delete asset; 
    }
}
#endif
