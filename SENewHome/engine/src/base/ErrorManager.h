#ifndef OMS_ERRORMANAGER_H
#define OMS_ERRORMANAGER_H
#include <string>
namespace oms
{
	class ErrorManager
	{
	public:
		ErrorManager();
		enum Error {OUT_OF_MEMORY, ASSERT_ERROR};
		void sendError(Error e, const std::string& msg);
	private:
		OMS_DECLARE_NO_COPY(ErrorManager);
	};
}
#endif
