module Meta.JavaServlet where

import qualified Meta.Java as J

-- * Classes

c_HttpServlet :: J.Class
c_HttpServlet = J.defClass {
        J.cPkg = "javax.servlet.http"
        , J.cName = "HttpServlet"
    }

c_HttpServletRequest :: J.Class
c_HttpServletRequest = J.defClass {
        J.cPkg = "javax.servlet.http"
        , J.cName = "HttpServletRequest"
    }

c_HttpServletResponse :: J.Class
c_HttpServletResponse = J.defClass {
        J.cPkg = "javax.servlet.http"
        , J.cName = "HttpServletResponse"
    }

c_IOException :: J.Class
c_IOException = J.defClass {
        J.cPkg = "java.io"
        , J.cName = "IOException"
    }

c_ServletException :: J.Class
c_ServletException = J.defClass {
        J.cPkg = "javax.servlet"
        , J.cName = "ServletException"
    }

-- * Methods

m_service :: J.Method
m_service = J.defMethod {
        J.mName = "service"
        , J.mParams = [
            J.mkParam (J.typeOf c_HttpServletRequest) "request"
            , J.mkParam (J.typeOf c_HttpServletResponse) "response"
        ]
        , J.mThrows = [
            J.typeOf c_IOException
            , J.typeOf c_ServletException
        ]
        , J.mAccess = J.Public
    }
