#ifndef RESOURCE_MANAGER_H
#define RESOURCE_MANAGER_H
#include <string>
namespace oms
{
	class ResourceManager
	{
	public:
		ResourceManager();
		void setDataPath(const std::string& dataPath)
		{
			mDataPath = dataPath;
		}
		const std::string& getDataPath()
		{
			return mDataPath;
		}
		void setResourceLoader(const ResourceType& resourceType, ResourceLoader* loader);
		ResourceLoader* getResourceLoader(const ResourceType& resourceType);
		void setResource(const ResourceType& resourcdType, const ResourceID& resourceID, Resource* res);
		Resource* getResource(const ResourceType& resourceType , const ResourceID& resourceID);
    private:
		std::string mDataPath;
	};
}
#endif
