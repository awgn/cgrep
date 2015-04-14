// -*- C++ -*-
//===-------------------------- typeinfo ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//


namespace std  /* purposefully not using versioning namespace */
{

class _LIBCPP_EXCEPTION_ABI type_info
{
    type_info& operator=(const type_info&);
    type_info(const type_info&);
};

}

