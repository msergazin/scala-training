package training1;

public class Book {
    Book(){}
    Book(int rating) {
        this.rating = rating;
    }
    Book(int rating, String name) {
        this(rating);
//        this.rating = rating;
        this.name = name;
    }

    private int rating;
    private String name;
    public int getRating() {return rating;}
    public String getName() {return name;}



    public static void main(String[] args) {
        Book bookEmpty = new Book();
        Book book = new Book(3);
        System.out.println(book.getRating());
        System.out.println(bookEmpty.getRating());
    }
}
