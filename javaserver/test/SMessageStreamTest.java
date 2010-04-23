import com.streamserver.util.SMessageStream;
import junit.framework.*;
public class SMessageStreamTest extends TestCase
{
    public SMessageStreamTest(String name)
    {
        super(name);
    }
    public void testCreate()
    {
        SMessageStream ss = new SMessageStream();
        byte[] data = new byte[10];
        data[0] = 1;
        short len = 10;
        //String lenStr = Short.toString(len);
	byte[] lenBytes =  new byte[2];//lenStr.getBytes();
	lenBytes[0] = (byte)((len >> 8) & 0xff);
	lenBytes[1] = (byte)(len & 0x00ff);
	System.arraycopy(lenBytes, 0, data, 1, 2);
        for(byte i = 3 ; i < 7 ; i++)
        {
            data[i] = i;
        }
        ss.addMessagePacket(data);
	System.out.println("ttt");
        SMessageStream.SMessage sm = new SMessageStream.SMessage();
        System.out.println("ttt");

        int ret = ss.getNextMessage(sm);
        System.out.println("ttt");
        for(int i = 0 ; i < sm.data.length; i++)
        {
            System.out.println(sm.data[i]);
        }
        assertEquals(ret, SMessageStream.S_NO_ERROR);
    }
}
