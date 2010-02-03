package com.streamserver.util;
import java.util.ArrayList;
import java.lang.RuntimeException;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
// this class is the container of message
// message can be an byte array of int , short or other format
// so message is big endian, when you create message data, it must be big endian
public class SMessageStream
{
    public static class SMessage 
    {
        public byte[] data;
    }
    private static class SMessagePacket
    {
        public byte[] data;
        public int len;
        public int offset;	
    }
    public static final int S_NO_ERROR = 0;
    public static final int S_WAIT_MORE = 1;
    public SMessageStream()
    {
    }
    public int getNextMessage(SMessage outMessage)
    {
        int ret = S_WAIT_MORE;
	byte[] outData = null;
	if(outMessage == null)
	    throw new RuntimeException("out message is null");
	synchronized(mMessagePacketList)
	{
	    if(!mMessagePacketList.isEmpty())
	    {
	        int dataLen = getDataLen();
		System.out.println("### data len = " + dataLen);
	        if(dataLen != 0)
		{
		    outData = new byte[dataLen];
		    int[] offsetBackup = new int[mMessagePacketList.size()];
                    backupOffset(offsetBackup);
		    int leftDataLen = dataLen;
		    int outDataIndex = 0;
		    int i = 0;
		    Iterator<SMessagePacket> it = mMessagePacketList.iterator();
		    //for(int i = 0 ; i < mMessagePacketList.size() ; i+=)
		    while(it.hasNext())
		    {
		        SMessagePacket mp = it.next();//mMessagePacketList.get(i);
			int packetLen = mp.len - mp.offset;
			if(packetLen >= leftDataLen)
			{
			    System.arraycopy(mp.data, mp.offset, outData, outDataIndex, leftDataLen);
			    offsetBackup[i] += leftDataLen;
			    leftDataLen = 0;
			    break;
			}
			else
			{
			    System.arraycopy(mp.data, mp.offset, outData, outDataIndex, packetLen);
			    leftDataLen -= packetLen;
			    offsetBackup[i] += packetLen;
			    outDataIndex += packetLen;
			}
			i++;
		    }
		    if(leftDataLen == 0)
		    {
		        ret = S_NO_ERROR;
		        changeOffset(offsetBackup);	
		    }
		    else
		    {
		        ret = S_WAIT_MORE;
		    }
                    //delete data packet from message list
		    List<SMessagePacket> newMessagePacketList = new LinkedList<SMessagePacket>();
		    it = mMessagePacketList.iterator();
		    while(it.hasNext())
		    {
		        SMessagePacket mp = it.next();
			if(mp.offset < mp.len)
			{
		            newMessagePacketList.add(mp);
			}
		    }
		    mMessagePacketList.clear();
		    mMessagePacketList = newMessagePacketList;
		    //end
		}
	        	
	    }
	}
	if(ret == S_NO_ERROR)
	{
	    outMessage.data = outData; 
	}
	return ret;
    }
    private void backupOffset(int[] offsets)
    {
        Iterator<SMessagePacket> it = mMessagePacketList.iterator();
        int i = 0;
        //for(int i = 0 ; i < mMessagePacketList.size() ; i++)
	while(it.hasNext())
	{
	    SMessagePacket mp = it.next();
	    offsets[i] = mp.offset;//mMessagePacketList.get(i).offset;
	    i++;
	}
    }
    private void changeOffset(int[] offsets)
    {
	Iterator<SMessagePacket> it = mMessagePacketList.iterator();
	int i = 0;
        //for(int i = 0 ; i < mMessagePacketList.size() ; i++)
	while(it.hasNext())
	{
	    SMessagePacket mp = it.next();
	    mp.offset = offsets[i];
	    i++;
	}

    }
    public int addMessagePacket(byte[] data)
    {
        if(data == null)
	    throw new RuntimeException("data is null pointer");
	SMessagePacket mp = new SMessagePacket();
	mp.data = data;
	mp.len = data.length;
	mp.offset = 0;
	synchronized(mMessagePacketList)
	{
	    mMessagePacketList.add(mp);
	}
	return S_NO_ERROR;
    }
    public int getMessagePacketCount()
    {
        int count = 0 ; 
	synchronized(mMessagePacketList)
	{
            count = mMessagePacketList.size();
	}
	return count;
    }
    private int getDataLen()
    {
	Iterator<SMessagePacket> it = mMessagePacketList.iterator();
        SMessagePacket front = null;
	if(it.hasNext())
	    front = it.next();
	if(front.offset >= front.len)
	    throw new RuntimeException("offset is greater than len");
	int leftDataLen = front.len - front.offset;
	short dataLen = 0;
	if(leftDataLen >= 3)
	{
	    byte[] tmpData = new byte[2];
	    System.arraycopy(front.data, front.offset + 1, tmpData, 0, 2);
	    int high = tmpData[0] & 0xff;
	    int low = tmpData[1] & 0xff;
	    dataLen = (short)((high << 8) | low);
	    return dataLen;
	}
	else
	{
	    byte[] headerData = new byte[3];
	    int currentLen = front.len - front.offset;
	    System.arraycopy(front, front.offset, headerData, 0, currentLen);
            leftDataLen = 3 - currentLen;
	    it = mMessagePacketList.iterator();
	    it.next();
	    //for(int i = 1 ; i < mMessagePacketList.size() ; i++)
	    while(it.hasNext())
	    {
	        SMessagePacket mp = it.next();
		if(mp.offset >= mp.len)
		    throw new RuntimeException("offset is greater than len");
		int packetCurrLen = mp.len - mp.offset;
		if(leftDataLen <= packetCurrLen)
		{
		    System.arraycopy(mp.data, mp.offset, headerData, currentLen, leftDataLen);
		    break;
		}
		else
		{
		    System.arraycopy(mp.data, mp.offset, headerData, currentLen, packetCurrLen);
		    leftDataLen -= packetCurrLen;
		    currentLen += packetCurrLen;
		}
	    }
	    if(leftDataLen == 0)
	    {
                byte[] tmpData = new byte[2];
                System.arraycopy(headerData, 1, tmpData, 0, 2);
                int high = tmpData[0] & 0xff;
  	        int low = tmpData[1] & 0xff;
	        dataLen = (short)((high << 8) | low);	
	        return dataLen;	
	    }
	    else
	    {
	        return 0;
	    }
	}
    }

    private List<SMessagePacket> mMessagePacketList = new LinkedList<SMessagePacket>(); 
}
