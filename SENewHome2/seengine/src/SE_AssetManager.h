#ifndef SE_ASSETMANAGER_H
#define SE_ASSETMANAGER_H
#ifdef ANDROID
#include <utils/Asset.h>
#include <utils/AssetManager.h>
class SE_AssetManager
{
public:
    virtual ~SE_AssetManager() {
        delete mAssetManager;
    }
    SE_AssetManager();
    SE_AssetManager(android::AssetManager* am);
    void readAsset(const char* fileName, char*& outData, int& outLen);
    //void addAssetPath(const android::String8& path);
    //void rmAssetPath(const android::String8& path);
private:
    void readLargeAsset(const char* fileName, char*& outData, int& outLen);
    android::Asset* openAsset(const char* fileName, int mode);
    android::AssetManager* mAssetManager;
  
};
#endif
#endif
