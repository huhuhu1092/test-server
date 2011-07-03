#ifndef OMS_CBFFILE_H
#define OMS_CBFFILE_H
#include "base/Type.h"
#include "resource/ResourceType.h"
namespace oms
{
    class ResourceManager;
    class CbfFile
    {
    public:
        struct Header
        {
            uint16_t magic;
            uint8_t versionEndian;
            uint16_t chunkNum;
        };
        struct ChunkHeader
        {
            uint16_t id;
            uint32_t size;
            std::string name;
            uint32_t offset;
        };
        class CbfLoader : public ResourceLoader
        {
        public:
            CbfLoader(InputBuffer& inputBuffer, ResourceManager* resourceManager) : ResourceLoader(inputBuffer, resourceManager)
            {}
            void read();
        };
        //////////////////////////
        CbfFile(const std::string& filePath, ResourceManager* resourceManager);
        void loadResource(ResourceType t, ResourceID resourceID);
        void loadAllResource();
        void loadHeader();
        void setCbfLoader(const ResourceType& resourceType, ResourceLoader* loader);
		ResourceLoader* getCbfLoader(const ResourceType& resourceType)
    private:
        Header mCbfHeader;
        std::vector<ChunkHeader> mChunkHeaderList;
        ResourceManager* mResourceManager;
    };
}
#endif
