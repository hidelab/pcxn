<%@page contentType="text/html; charset=UTF-8"%>
<%@page import="org.json.simple.JSONArray"%>
<%@page import="org.json.simple.JSONObject"%>
<%@page import="java.sql.*"%>
<%@page import="java.util.Vector"%>
<%@page import="java.util.Iterator"%>
<%@page import="java.io.FileNotFoundException"%>
<%@page import="java.io.IOException"%>
<%@page import="java.io.InputStream"%>
<%@page import="java.util.Properties"%>

<%
	String geneSetCollectionValue = request.getParameter("geneSetCollection");	
	String tblName = "genesetname_genesetdb_tbl";
	
	String JDBC_DRIVER = "com.mysql.jdbc.Driver";  
	String DB_URL = "jdbc:mysql://localhost/pcxn_database";

	//  Database credentials
	String USER = "";
	String PASS = "";
	try {
		Properties prop = new Properties();
		String propFileName = "pcxnconfig.properties"; 
		InputStream inputStream = getClass().getClassLoader().getResourceAsStream(propFileName); 
		if (inputStream != null) {
			prop.load(inputStream);
		} else {
			throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath");
		}
		USER = prop.getProperty("user");
		PASS = prop.getProperty("password");
	} catch (Exception e) {
		out.println("Exception: " + e);
	} finally {
	}
   
	Connection conn = null;
	Statement stmt = null;
	JSONObject obj=new JSONObject();
	String [] genesetdbs = {"pathprint", "MSigDBC2CP", "MSigDBC5", "MSigDBH"}; 	
	int size = genesetdbs.length;
    for (int i=0; i<size; i++)
    {        
		String sql1 = "select geneset_name from genesetname_genesetdb_tbl where geneset_db = '" + genesetdbs[i] + "'";
		JSONArray list = new JSONArray();
		try{
			//STEP 2: Register JDBC driver
			Class.forName("com.mysql.jdbc.Driver");

			//STEP 3: Open a connection
			conn = DriverManager.getConnection(DB_URL,USER,PASS);

			//STEP 4: Execute a query
			stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql1);

			//STEP 5: Extract data from result set
			while(rs.next()){
				//Retrieve by column name
				String geneset_name = rs.getString("geneset_name");

				//store values			
				list.add(geneset_name);
			}
			//STEP 6: Clean-up environment
			rs.close();
			stmt.close();
			conn.close();
		}catch(SQLException se){
			//Handle errors for JDBC
			se.printStackTrace();
		}catch(Exception e){
			//Handle errors for Class.forName
			e.printStackTrace();
		}finally{
			//finally block used to close resources
			try{
				if(stmt!=null)
					stmt.close();
			}catch(SQLException se2){
			}// nothing we can do
			try{
         		if(conn!=null)
            		conn.close();
			}catch(SQLException se){
				se.printStackTrace();
			}
		}//end finally try
		obj.put(genesetdbs[i],list);
	}//end try
	out.print(obj);
	out.flush();
%>