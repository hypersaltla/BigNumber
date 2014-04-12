#include<iostream>
#include<string>
#include<cstring>

using namespace std;
const int maxn=100;

class BigInteger
{
	public:
    
    BigInteger()
    {
        init();
    }
    ~BigInteger()
    {
        clear();
    }
    BigInteger(const char* other)
    {
        init();
        this->copy(other);
    }
    
    BigInteger(const string& other)
    {
        init();
        this->copy(other);
    }
    
    BigInteger(const BigInteger& n)
    {
        init();
        this->copy(n);
    }
    
    BigInteger& operator=(const char* other)
	{
        clear();
        this->copy(other);
		return *this;
	}
    
    BigInteger& operator=(const string& other)
    {
        clear();
        this->copy(other);
        return *this;
    }
    
    BigInteger& operator=(const BigInteger& other)
    {
        clear();
        this->copy(other);
        return *this;
    }
    
	string str() const
	{
		string res;
        if (!sign) {
            if (len > 1 || num[0] != 0) {
                res.push_back('-');
            }
        }
		for(int i = len - 1; i >= 0; i--)
			res.push_back( num[i] + '0');
		return res;
	}
     
	friend istream& operator>>(istream &in, BigInteger& x)
	{
		string s;
		in >> s;
		x = s;
		return in;
	}
	friend ostream& operator<<(ostream &out, BigInteger& x)
	{
		out << x.str();
		return out;
	}
    
    BigInteger add(const BigInteger &other) const
    {
        if (sign ^ other.sign) {
            return sign ? this->subtract(other.get_neg()) : other.subtract(this->get_neg());
        }
        BigInteger new_int;
        new_int.len = max(len, other.len) + 1;
        new_int.num = new char[new_int.len];
        new_int.sign = sign;
        char carry = 0;
        
		for(int i = 0; i < new_int.len; i++)
		{
			char digit = carry;
			if( i < len ) digit += num[i];
			if( i < other.len) digit += other.num[i];
			new_int.num[i] = digit % 10;
			carry = digit / 10;
		}
        if (new_int.num[new_int.len - 1] == 0) { //if most significant digit is zero
            new_int.len--;
        }
        
		return new_int;
    }
    
    BigInteger subtract(const BigInteger &other) const
    {
        if (sign ^ other.sign) {
            return this->add(other.get_neg());
        }
        if (this->get_abs().compare(other.get_abs()) < 0) {
            return other.subtract(*this).get_neg();
        }
        //retains the contion that abs(num) > abs(other)
        BigInteger new_int;
        new_int.len = len;
        new_int.num = new char[new_int.len];
        new_int.sign = sign;
        bool borrow = false;
        
        for (int i = 0; i < len; i++) {
            char digit = num[i];
            if (borrow) {
                if (digit > 0) {
                    digit--;
                    borrow = false;
                }
                else {
                    digit = 9;
                }
            }
            if (i < other.len) {
                if (digit >= other.num[i]) {
                    digit -= other.num[i];
                }
                else {
                    borrow = true;
                    digit = digit + 10 - other.num[i];
                }
            }
            new_int.num[i] = digit;
        }
        //trim prefix zeros
        while (new_int.len > 1 && new_int.num[new_int.len - 1] == 0) new_int.len--;
        return new_int;
    }
    
    BigInteger multiply(const BigInteger &other) const
    {
        BigInteger new_int;
        new_int.len = len + other.len;
        new_int.num = new char[new_int.len];
        new_int.sign = !(sign ^ other.sign);
        memset(new_int.num, 0, sizeof(char) * new_int.len);
        for (int i = 0; i < len; i++) {
            char c_mul = 0;
            char c_add = 0;
            for (int j = 0; j < other.len; j++) {
                char mul = num[i] * other.num[j] + c_mul;
                char add = mul % 10 + new_int.num[i + j] + c_add;
                new_int.num[i + j] = add % 10;
                c_mul = mul / 10;
                c_add = add / 10;
            }
            new_int.num[i + other.len] = c_add + c_mul;
        }
        //trim prefix zeros
        while (new_int.len > 1 && new_int.num[new_int.len - 1] == 0) new_int.len--;
        return new_int;
    }
    
    //"this" divide "other", "other" / "this"
    //"this" is zero, may lead to undefined behavior
    BigInteger divide(const BigInteger &other) const
    {
        BigInteger new_int;
        if (len == 1 && (num == NULL || num[0] == 0)) {
            return new_int;
        }
        new_int.len = other.len - len + 1;
        if (new_int.len <= 0 || (other.len == 1 && other.num[0] == 0)) {
            return new_int = "0";
        }
        new_int.num = new char[new_int.len];
        memset(new_int.num, 0, sizeof(char) * new_int.len);
        new_int.sign = !(sign ^ other.sign);
        
        BigInteger remainder = other.get_abs();
        BigInteger dividor = this->get_abs();
        //initialize the subtr to be the same size of remainder
        BigInteger subtr;
        subtr.len = remainder.len;
        subtr.num = new char[remainder.len];
        for (int i = subtr.len - 1, j = dividor.len - 1; i >= 0; i--, j--) {
            if(j >= 0) subtr.num[i] = num[j];
            else subtr.num[i] = 0;
        }
        int msb = new_int.len - 1;
        while (remainder.compare(dividor) >= 0) {
            char quotient = 0;
            while (remainder.compare(subtr) >= 0) {
                remainder = remainder.subtract(subtr);
                quotient++;
            }
            new_int.num[msb--] = quotient;
            //right shift subtr by one digit
            if (subtr.num[0] == 0) {
                for (int i = 1; i < subtr.len; i++) {
                    subtr.num[i - 1] = subtr.num[i];
                }
                subtr.len--;
            }
        }
        while (new_int.len > 1 && new_int.num[new_int.len - 1] == 0) new_int.len--;
        if (!new_int.sign && !(remainder.len == 1 && remainder.num[0] == 0)) { //flooring negative value
            new_int.decrement();
        }
        return new_int;
    }
    
    
    BigInteger& increment()
    {
        BigInteger one("1");
        return *this = this->add(one);
    }
    
    BigInteger& decrement()
    {
        BigInteger one("1");
        return *this = this->subtract(one);
    }
    
    int compare(const BigInteger &other) const
    {
        if (sign ^ other.sign) {
            return sign ? 1 : -1;
        }
        if (len > other.len) {
            return sign ? 1 : -1;
        }
        if (len < other.len) {
            return sign ? -1 : 1;
        }
        for (int i = len - 1; i >= 0; i--) {
            if (num[i] != other.num[i]) {
                return sign ? num[i] - other.num[i] : other.num[i] - num[i];
            }
        }
        return 0;
    }
    
    BigInteger get_neg() const
    {
        BigInteger new_int(*this);
        new_int.sign = !new_int.sign;
        return new_int;
    }
    
    BigInteger get_abs() const
    {
        BigInteger new_int(*this);
        new_int.sign = true;
        return new_int;
    }
                                         
	BigInteger& operator+=(const BigInteger &other)
	{
		return *this = this->add(other);
	}
    BigInteger& operator-=(const BigInteger &other)
	{
		return *this = this->subtract(other);
	}
    BigInteger& operator*=(const BigInteger &other)
    {
        return *this = this->multiply(other);
    }
    BigInteger& operator/=(const BigInteger &other)
    {
        return *this = other.divide(*this);
    }
    
	BigInteger& operator++()
	{
		return this->increment();
	}
	const BigInteger operator++(int)
	{
		BigInteger c = *this;
        increment();
		return c;
	}
	
	BigInteger& operator--()
	{
		return this->decrement();
	}
	const BigInteger operator--(int)
	{
		BigInteger c = *this;
		decrement();
		return c;
	}
    private:
    
    //the length of the integer
    int len;
    //the sign of the integer
    bool sign;
    //the char array representing the big integer, start from the right (num[len]) which is the most significant digit.
    //note: it is not a string, so it does not have null terminator.
    char *num;
    //copy the integer from a string in a natual representation, from left to right
    void copy(const string &other)
    {
        if (other.length() == 0) {
            return;
        }
        if (other[0] == '-') {
            sign = false;
        }
        len = sign ? other.length() : other.length() - 1;
        num = new char[len];
        
        for(int i = 0; i < len; i++) {
            num[i] = other[other.length() - i - 1] - '0';
        }
        //trim prefix zeros
        while (len > 1 && num[len - 1] == 0) len--;
    }
    void copy(const char* other)
    {
        string str(other);
        copy(str);
    }
    void copy(const BigInteger& other)
    {
        len = other.len;
        sign = other.sign;
        num = new char[len];
        for (int i = 0; i < len; i++) {
            num[i] = other.num[i];
        }
    }
    void init()
    {
        len = 0;
        sign = true;
        num = NULL;
    }
    void clear()
    {
        if (len != 0 && num) {
            delete num;
        }
        len = 0;
        sign = true;
    }
};

BigInteger operator+(const BigInteger &n1, const BigInteger &n2)
{
    return n1.add(n2);
}

BigInteger operator-(const BigInteger &n1, const BigInteger &n2)
{
    return n1.subtract(n2);
}

BigInteger operator*(const BigInteger &n1, const BigInteger &n2)
{
    return n1.multiply(n2);
}

BigInteger operator/(const BigInteger &n1, const BigInteger &n2)
{
    return n2.divide(n1);
}

bool operator>(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) > 0;
}

bool operator<(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) < 0;
}

bool operator>=(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) >= 0;
}

bool operator<=(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) <= 0;
}

bool operator==(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) == 0;
}

bool operator!=(const BigInteger &n1, const BigInteger &n2)
{
    return n1.compare(n2) != 0;
}


int main()
{
	BigInteger a,b, zero("00001234153");
	while( cin >> a >> b ){
        //cout << a << endl;
        a /= b;
        
		//BigInteger k = b, c;
        //k = "-123456";
        //cout << k << endl;
        //cout << zero << endl;
		//BigInteger x = a + --b;
        //BigInteger y = --a + b;
        //BigInteger y = b.get_neg();
		//k=a/b;
		//a=c*b+k;
		//cout<<a<<endl;
		//cout<<x<<' '<<y<<' '<<endl;
        cout<< a <<endl;
	}
    return 0;
}
