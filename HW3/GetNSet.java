import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;
    // private int swapCount = 0; debugging purposes

    private AtomicIntegerArray byteArrToAtomIntArr(byte[] arr) {
        int arrLen = arr.length;
        int[] intArr = new int[arrLen];
        for (int i = 0; i < arrLen; i++) {
            intArr[i] = arr[i];
        }
        AtomicIntegerArray r = new AtomicIntegerArray(intArr);
        return r;
    }

    GetNSet(byte[] v) {
        value = byteArrToAtomIntArr(v);
        maxval = 127;
    }

    GetNSet(byte[] v, byte m) {
        value = byteArrToAtomIntArr(v);
        maxval = m;

    }

    public int size() { return value.length(); }

    public byte[] current() {
        //System.out.println("current value: " + value);
        int arrLen = size();
        byte[] r = new byte[arrLen];
        for (int i = 0; i < arrLen; i++) {
            r[i] = (byte) value.get(i);
        }
        return r;
    }

    public boolean swap(int i, int j) {
        //System.out.println("Swap count: " + swapCount++);
        int val_i = value.get(i);
        int val_j = value.get(j);
    	if (val_i <= 0 || val_j >= maxval) {
    	    return false;
    	}
    	value.set(i, val_i - 1);
    	value.set(j, val_j + 1);
        //System.out.println("Value after swap: " + value);
    	return true;
    }
}