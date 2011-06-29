#ifndef OMS_RESOURCE_H
#define OMS_RESOURCE_H
namespace oms
{
	class Resource
	{
	public:
		Resource(const ResourceType& type, const ResourceID& id);
		ResourceType getType();
		void setType(const ResourceType& t);
		ResourceID getID();
		void setID(const ResourceID& id);
		virtual ~Resource();
	private:
		ResourceType mType;
		ResourceID mID;
	};
}

#endif
