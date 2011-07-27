#ifndef OMS_RENDERSTATE_H
#define OMS_RENDERATATE_H
namespace oms
{
	class RenderState
	{
	public:
		enum Type {INVALID, TEXTURE, SHADER, DEPTH, ALPHA, BLEND, STENCIL, MATERIAL, NUM};
		RenderState(Type t = INVALID)
		{
			mType = t;
		}
		void setType(Type t)
		{
			mType = t;
		}
		Type getType() const
		{
			return mType;
		}
		virtual ~RenderState();
		virtual void apply();
		//-1 : <
		//0 : ==
		//1 : >
		virtual int compare(RenderState* rs);
	private:
		Type mType;
	};
}
#endif
