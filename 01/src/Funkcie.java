import java.awt.List;
import java.util.ArrayList;

public class Funkcie {

	public static void foo(FunkcionalnyInterface fi) {
		fi.doit("hello");
	}

	public static FunkcionalnyInterface goo() {
		return (String s) -> System.out.println(s + s);
	}

	public static String foo1(FunkcionalnyInterface1 fi) {
		return fi.doit("hello");
	}

	public static FunkcionalnyInterface1 goo1() {
		return
			(String s)->(s+s);
	}
	//	----------------
	public static RealnaFunkcia iterate(int n, RealnaFunkcia f) {
		if (n == 0)
			return (double d)->d;
		else {			
			RealnaFunkcia rf = iterate(n-1, f);
			return (double d)->f.doit(rf.doit(d));
		}
	}
	
	public static void main(String[] args) {
		foo(goo());
		System.out.println(foo1(goo1()));
		
		RealnaFunkcia rf = iterate(5, (double d)->d*2);
		System.out.println(rf.doit(1));


		// ArrayList list = new ArrayList<Integer>();
		// list.forEach((Integer i) -> i+5);

	}
}
