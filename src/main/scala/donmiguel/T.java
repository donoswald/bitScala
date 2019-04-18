package donmiguel;

import scala.Int;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

public class T {
    public static void main(String[] args) {


        for(int i=0; i<256;i++ ){
            System.out.println(i);
            System.out.println(Integer.toBinaryString((byte)i));
            System.out.println(Integer.toHexString((byte)i));
        }


        //System.out.println(v);



//        ByteBuffer byteBuffer = ByteBuffer.allocate(4);
//        byteBuffer.order(ByteOrder.BIG_ENDIAN);
//        byteBuffer.putInt(88);
//        byte[] result = byteBuffer.array();
//        System.out.println(Arrays.toString(result));
//
//        ByteBuffer byteBuffer2 = ByteBuffer.allocate(4);
//        byteBuffer2.order(ByteOrder.LITTLE_ENDIAN);
//        byteBuffer2.putInt(88);
//        byte[] result2 = byteBuffer2.array();
//        System.out.println(Arrays.toString(result2));






    }
}
