/**
 * A point object with an x and y value
 * used to create a coordinate with a specific label.
*/

CREATE OR REPLACE TYPE POINT AS OBJECT(
 X     INTEGER,
 Y     INTEGER,
 LABEL CHAR(1),
 MEMBER FUNCTION get_X RETURN INTEGER,
 MEMBER FUNCTION get_Y RETURN INTEGER,
 MEMBER FUNCTION get_Label RETURN CHAR,
 CONSTRUCTOR FUNCTION POINT
   (X INTEGER,
    Y INTEGER,
    LABEL CHAR) RETURN SELF as RESULT);
    

/**
 * Function bodies for POINT object.
*/
 
CREATE OR REPLACE TYPE BODY POINT AS

    /** Gets the x-value of point.
     * @return The integer x-value of the point.
    */
    MEMBER FUNCTION get_X RETURN INTEGER
    IS
    BEGIN
      RETURN X;
    END;
    
    /** Gets the y-value of point.
     * @return The integer y-value of the point.
    */
    MEMBER FUNCTION get_Y RETURN INTEGER
    IS
    BEGIN
      RETURN Y;
    END;
    
    /** Gets the label of point.
     * @return The single character used for point label.
    */
    MEMBER FUNCTION get_Label RETURN CHAR
    IS
    BEGIN
      RETURN LABEL;
    END;
    
    /** Creates a point with the specified label and x-y cooordinate.
     * @param X     The point’s x-value.
     * @param Y     The point’s y-value.
     * @param LABEL The point’s label.
     * @return The point object created.
    */
    CONSTRUCTOR FUNCTION POINT
      (X INTEGER,
       Y INTEGER,
       LABEL CHAR) RETURN SELF as RESULT
       IS
        BEGIN
            self.x := x;
            self.y := y;
            self.label := label;
            
            IF self.x < 0 OR self.y < 0 THEN
              RAISE_APPLICATION_ERROR(-20000, 'Coordinate not in upper right quadrant.');
            END IF;
            
            RETURN;
        END;
END;


/**
 * Table to hold POINT objects.
*/
 
CREATE TABLE POINTS of POINT(
 unique(x, y),
 unique(label));

/*
 *Creating and inserting POINT objects.
*/
INSERT INTO POINTS 
VALUES(POINT(1,1,'A'));

INSERT INTO POINTS 
VALUES(POINT(1,2,'B'));

INSERT INTO POINTS 
VALUES(POINT(2,1,'C'));

INSERT INTO POINTS 
VALUES(POINT(2, 2,'D'));

INSERT INTO POINTS 
VALUES(POINT(3, 3,'E'));

INSERT INTO POINTS 
VALUES(POINT(3, 4,'F'));

INSERT INTO POINTS 
VALUES(POINT(1, 4,'G'));

INSERT INTO POINTS 
VALUES(POINT(4, 1,'H'));


/*
 * A line object composed of two POINT object references.
*/

CREATE OR REPLACE TYPE LINE AS OBJECT(
 POINT1  REF POINT,
 POINT2  REF POINT,
 MEMBER FUNCTION get_Slope(p1 POINT, p2 POINT) RETURN INTEGER,
 MEMBER FUNCTION get_Orientation RETURN CHAR,
 MEMBER FUNCTION get_Length RETURN INTEGER,
 CONSTRUCTOR FUNCTION LINE(
 POINT1  REF POINT,
 POINT2  REF POINT) RETURN SELF as RESULT);
 
CREATE OR REPLACE TYPE BODY LINE AS

    /** Calculates the slope of two points.
     * @param  p1     Point object.
     * @param  p2     Point object.
     * @return The integer value of the slope (NULL if slope undefined).
    */
    MEMBER FUNCTION get_Slope(p1 POINT, p2 POINT) 
    RETURN INTEGER
    IS
        slope INTEGER;
    BEGIN
        -- Slope Formula
        slope := (p2.get_Y() - p1.get_Y()) / NULLIF(p2.get_X() - p1.get_X(), 0);
        RETURN slope;
    END;
    
    /** Calculates the length of LINE object.
     * @return The integer value of the distance.
    */
    MEMBER FUNCTION get_Length RETURN INTEGER
    IS
        p1 POINT;
        p2 POINT;
        length INTEGER;
        
    BEGIN
        select deref(self.point1) into p1 from dual;
        select deref(self.point2) into p2 from dual;
        
        --Distance Formula
        length := SQRT(POWER(p2.get_Y() - p1.get_Y(), 2) + POWER(p2.get_X() - p1.get_X(), 2));
        RETURN length;
    END;
    
    /** Determines the orientation of LINE object.
     * The orientation will be either horizontal ('H') or vertical ('V').
     * @return The character value of either 'H' or 'V'.
    */
    MEMBER FUNCTION get_Orientation RETURN CHAR
    IS
        p1 POINT;
        p2 POINT;
        slope INTEGER;
    BEGIN
    
        select deref(self.point1) into p1 from dual;
        select deref(self.point2) into p2 from dual;
        
        slope := self.get_Slope(p1,p2);
        
        IF
            slope = 0 THEN
            RETURN 'H';
        END IF;
        
        IF
            slope IS NULL THEN
            RETURN 'V';
        END IF;
    END;
    
    /** Creates a LINE object given two point references.
     * @param POINT1    A reference to a POINT object.
     * @param POINT2    A reference to a POINT object.
     * @return The line object created.
    */
    CONSTRUCTOR FUNCTION LINE(
    POINT1  REF POINT,
    POINT2  REF POINT) RETURN SELF as RESULT
    IS
        p1 POINT;
        p2 POINT;
        slope INTEGER;
        
    BEGIN
        self.point1 := point1;
        self.point2 := point2;
        
        select deref(self.point1) into p1 from dual;
        select deref(self.point2) into p2 from dual;
        
        -- Slope
        slope := self.get_Slope(p1,p2);
        
        IF
            NVL(slope, 0) <> 0 THEN
            RAISE_APPLICATION_ERROR(-20000, 'Not a valid line. Line must be either parallel to X axis or parallel to Y axis.');
        END IF;
        
        RETURN;
    END;
END;

/*
 * A rectangle object composed of four LINE objects.
*/
CREATE OR REPLACE TYPE RECTANGLE AS OBJECT(
 LEFT LINE,
 RIGHT LINE,
 TOP LINE,
 BOTTOM LINE,
 MEMBER FUNCTION get_Area RETURN INTEGER,
 MEMBER FUNCTION get_Perimeter RETURN INTEGER,
 CONSTRUCTOR FUNCTION RECTANGLE(
 LEFT LINE,
 RIGHT LINE,
 TOP LINE,
 BOTTOM LINE) RETURN SELF as RESULT);
 
CREATE OR REPLACE TYPE BODY RECTANGLE AS

    /** Calculates the area of RECTANGLE object.
     * @return The integer value of the area of rectangle.
    */
    MEMBER FUNCTION get_Area RETURN INTEGER
    IS
        horizontal_side INTEGER;
        vertical_side INTEGER;
    BEGIN
        -- LEFT Orientation check
        IF self.left.get_Orientation() = 'H' THEN
            horizontal_side := self.left.get_length();
        ELSE
            vertical_side := self.left.get_length();
        END IF;
        
        -- RIGHT Orientation check
        IF self.right.get_Orientation() = 'H' THEN
            horizontal_side := self.right.get_length();
        ELSE
            vertical_side := self.right.get_length();
        END IF;
        
        -- TOP Orientation check
        IF self.top.get_Orientation() = 'H' THEN
            horizontal_side := self.top.get_length();
        ELSE
            vertical_side := self.top.get_length();
        END IF;
        
        -- BOTTOM Orientation check
        IF self.bottom.get_Orientation() = 'H' THEN
            horizontal_side := self.bottom.get_length();
        ELSE
            vertical_side := self.bottom.get_length();
        END IF;
        
        -- Rectangle Area
        RETURN horizontal_side * vertical_side;
    END;
    
     /** Calculates the perimeter of RECTANGLE object.
     * @return The integer value of the perimeter of rectangle.
    */
    MEMBER FUNCTION get_Perimeter RETURN INTEGER
    IS
    BEGIN
        RETURN self.left.get_length() + self.right.get_length() + self.top.get_length() + self.bottom.get_length();
    END;
    
    /** Creates a RECTANGLE object given four LINE objects.
     * @param LINE1    A LINE object.
     * @param LINE2    A LINE object.
     * @param LINE3    A LINE object.
     * @param LINE4    A LINE object.
     * @return The RECTANGLE object created.
    */
    CONSTRUCTOR FUNCTION RECTANGLE(
    LEFT   LINE,
    RIGHT  LINE,
    TOP    LINE,
    BOTTOM LINE) RETURN SELF as RESULT
    IS
        TYPE ARRAY_LINES IS VARRAY(4) OF LINE;
        TYPE ARRAY_ORIENTATION IS TABLE OF LINE;
        
        line_arr ARRAY_LINES; -- Line Objects Array
        horizontal_arr ARRAY_ORIENTATION := ARRAY_ORIENTATION(); -- Horizontal Line Objects
        vertical_arr ARRAY_ORIENTATION := ARRAY_ORIENTATION(); -- Vertical Line Objects
        
        v_p1 POINT;
        v_p2 POINT;
        h_p1 POINT;
        h_p2 POINT;
        
    BEGIN
        self.left := left;
        self.right := right;
        self.top := top;
        self.bottom := bottom;
        
        line_arr := ARRAY_LINES(self.left, self.right, self.top, self.bottom);
        
        -- Determining lines orientation and putting into its respective orientation array
        FOR i IN 1..4 LOOP
            IF line_arr(i).get_Orientation() = 'H' THEN
                horizontal_arr.EXTEND;
                horizontal_arr(horizontal_arr.LAST) := line_arr(i);
            ELSE
                vertical_arr.EXTEND;
                vertical_arr(vertical_arr.LAST) := line_arr(i);
            END IF;
        END LOOP;
        
        --Rectangle validity check: 2 Horizontal Lines and 2 Vertical Lines.
        IF horizontal_arr.COUNT <> 2 or vertical_arr.COUNT <> 2 THEN
            RAISE_APPLICATION_ERROR(-20000, 'Not a valid rectanlge. Exactly two vertical lines and two horizontal lines are needed.');
        END IF;
        
        --Rectangle validity check: Both Horizontal Lines are same length.
        IF horizontal_arr(1).get_length() <> horizontal_arr(2).get_length() THEN
            RAISE_APPLICATION_ERROR(-20000, 'Not a valid rectanlge. Horizontal lines must be same length.');
        END IF;
        
        --Rectangle validity check: Both Vertical Lines are same length.
        IF vertical_arr(1).get_length() <> vertical_arr(2).get_length() THEN
            RAISE_APPLICATION_ERROR(-20000, 'Not a valid rectanlge. Vertical lines must be same length.');
        END IF;
        
        --Rectangle validity check: All lines are connected.
        FOR i IN 1..2 LOOP
            select deref(vertical_arr(i).point1) into v_p1 from dual;
            select deref(vertical_arr(i).point2) into v_p2 from dual;
            
            FOR j IN 1..2 LOOP
                select deref(horizontal_arr(j).point1) into h_p1 from dual;
                select deref(horizontal_arr(j).point2) into h_p2 from dual;
                
                IF v_p1.get_label() NOT IN (h_p1.get_label(), h_p2.get_label()) 
                   AND v_p2.get_label() NOT IN (h_p1.get_label(), h_p2.get_label())
                THEN 
                    RAISE_APPLICATION_ERROR(-20000, 'Not a valid rectanlge. Lines do not connect.');
                END IF;
                
            END LOOP;
            
        END LOOP;
        
        
        RETURN;
    END;

END;

/**
 * Table to hold RECTANGLE objects.
*/
CREATE TABLE BOXES of RECTANGLE;

drop table boxes purge;

/**
 * INSERTING RECTANGLE objects into BOXES.
*/
INSERT INTO BOXES
VALUES(
    RECTANGLE(
        LINE((select ref(A) from points A where label = 'A'), (select ref(A) from points A where label = 'B')),
        LINE((select ref(A) from points A where label = 'A'), (select ref(A) from points A where label = 'C')),
        LINE((select ref(A) from points A where label = 'C'), (select ref(A) from points A where label = 'D')),
        LINE((select ref(A) from points A where label = 'B'), (select ref(A) from points A where label = 'D'))
    )
);

-- Rectangle methods   
select A.get_perimeter() AS "RECTANGLE PERIMETER" from boxes A;
select A.get_area() AS "RECTANGLE AREA" from boxes A;

-- Line methods
select A.left.get_orientation() AS "LEFT ORIENTATION" from boxes A;
select A.right.get_orientation() AS "RIGHT ORIENTATION" from boxes A;
select A.top.get_orientation() AS "TOP ORIENTATION" from boxes A;
select A.bottom.get_orientation() AS "BOTTOM ORIENTATION" from boxes A;

-- Point Methods

-- LEFT LINE ENDPOINTS
select 
    A.left.point1.get_X() AS "LEFT LINE POINT1 X", 
    A.left.point1.get_Y() AS "LEFT LINE POINT1 Y", 
    A.left.point2.get_X() AS "LEFT LINE POINT2 X", 
    A.left.point2.get_Y() AS "LEFT LINE POINT2 Y"  
from boxes A;

-- RIGHT LINE ENDPOINTS
select 
    A.right.point1.get_X() AS "RIGHT LINE POINT1 X", 
    A.right.point1.get_Y() AS "RIGHT LINE POINT1 Y", 
    A.right.point2.get_X() AS "RIGHT LINE POINT2 X", 
    A.right.point2.get_Y() AS "RIGHT LINE POINT2 Y"  
from boxes A;

-- TOP LINE ENDPOINTS
select 
    A.top.point1.get_X() AS "TOP LINE POINT1 X", 
    A.top.point1.get_Y() AS "TOP LINE POINT1 Y", 
    A.top.point2.get_X() AS "TOP LINE POINT2 X", 
    A.top.point2.get_Y() AS "TOP LINE POINT2 Y"  
from boxes A;

-- BOTTOM LINE ENDPOINTS
select 
    A.bottom.point1.get_X() AS "BOTTOM LINE POINT1 X", 
    A.bottom.point1.get_Y() AS "BOTTOM LINE POINT1 Y", 
    A.bottom.point2.get_X() AS "BOTTOM LINE POINT2 X", 
    A.bottom.point2.get_Y() AS "BOTTOM LINE POINT2 Y"  
from boxes A;


