/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * NFSv3 constants and types
 */
const NFS3_FHSIZE   = 64;   /* maximum size in bytes of a file handle */
const NFS3_COOKIEVERFSIZE = 8;  /* size of a cookie verifier for READDIR */
const NFS3_CREATEVERFSIZE = 8;  /* size of the verifier used for CREATE */
const NFS3_WRITEVERFSIZE = 8;   /* size of the verifier used for WRITE */

typedef unsigned hyper uint64;
typedef hyper int64;
/* typedef unsigned long uint32; */
typedef unsigned uint32;    /* XXX? */
typedef int int32;      /* XXX? */
typedef string filename3<>;
typedef string nfspath3<>;
typedef uint64 fileid3;
typedef uint64 cookie3;
typedef opaque cookieverf3[NFS3_COOKIEVERFSIZE];
typedef opaque createverf3[NFS3_CREATEVERFSIZE];
typedef opaque writeverf3[NFS3_WRITEVERFSIZE];
typedef uint32 uid3;
typedef uint32 gid3;
typedef uint64 size3;
typedef uint64 offset3;
typedef uint32 mode3;
typedef uint32 count3;

/*
 * Error status (v3)
 */
enum nfsstat3 {
    NFS3_OK = 0,
    NFS3ERR_PERM        = 1,
    NFS3ERR_NOENT       = 2,
    NFS3ERR_IO      = 5,
    NFS3ERR_NXIO        = 6,
    NFS3ERR_ACCES       = 13,
    NFS3ERR_EXIST       = 17,
    NFS3ERR_XDEV        = 18,
    NFS3ERR_NODEV       = 19,
    NFS3ERR_NOTDIR      = 20,
    NFS3ERR_ISDIR       = 21,
    NFS3ERR_INVAL       = 22,
    NFS3ERR_FBIG        = 27,
    NFS3ERR_NOSPC       = 28,
    NFS3ERR_ROFS        = 30,
    NFS3ERR_MLINK       = 31,
    NFS3ERR_NAMETOOLONG = 63,
    NFS3ERR_NOTEMPTY    = 66,
    NFS3ERR_DQUOT       = 69,
    NFS3ERR_STALE       = 70,
    NFS3ERR_REMOTE      = 71,
    NFS3ERR_BADHANDLE   = 10001,
    NFS3ERR_NOT_SYNC    = 10002,
    NFS3ERR_BAD_COOKIE  = 10003,
    NFS3ERR_NOTSUPP     = 10004,
    NFS3ERR_TOOSMALL    = 10005,
    NFS3ERR_SERVERFAULT = 10006,
    NFS3ERR_BADTYPE     = 10007,
    NFS3ERR_JUKEBOX     = 10008
};

/*
 * File types (v3)
 */
enum ftype3 {
    NF3REG  = 1,        /* regular file */
    NF3DIR  = 2,        /* directory */
    NF3BLK  = 3,        /* block special */
    NF3CHR  = 4,        /* character special */
    NF3LNK  = 5,        /* symbolic link */
    NF3SOCK = 6,        /* unix domain sockets */
    NF3FIFO = 7     /* named pipe */
};

struct specdata3 {
    uint32  specdata1;
    uint32  specdata2;
};

/*
 * File access handle (v3)
 */
struct nfs_fh3 {
    opaque data<NFS3_FHSIZE>;
};

/* 
 * Timeval (v3)
 */
struct nfstime3 {
    uint32  seconds;
    uint32  nseconds;
};


/*
 * File attributes (v3)
 */
struct fattr3 {
    ftype3  type;       /* file type */
    mode3   mode;       /* protection mode bits */
    uint32  nlink;      /* # hard links */
    uid3    uid;        /* owner user id */
    gid3    gid;        /* owner group id */
    size3   size;       /* file size in bytes */
    size3   used;       /* prefered block size */
    specdata3 rdev;     /* special device # */
    uint64 fsid;        /* device # */
    fileid3 fileid;     /* inode # */
    nfstime3 atime;     /* time of last access */
    nfstime3 mtime;     /* time of last modification */
    nfstime3 ctime;     /* time of last change */
};

union post_op_attr switch (bool attributes_follow) {
case TRUE:
    fattr3  attributes;
case FALSE:
    void;
};

struct wcc_attr {
    size3   size;
    nfstime3 mtime;
    nfstime3 ctime;
};

union pre_op_attr switch (bool attributes_follow) {
case TRUE:
    wcc_attr attributes;
case FALSE:
    void;
};

struct wcc_data {
    pre_op_attr before;
    post_op_attr after2;    /* Changed to after2 to avoid Erlang syntax. */
};

union post_op_fh3 switch (bool handle_follows) {
case TRUE:
    nfs_fh3 handle;
case FALSE:
    void;
};

/*
 * File attributes which can be set (v3)
 */
enum time_how {
    DONT_CHANGE     = 0,
    SET_TO_SERVER_TIME  = 1,
    SET_TO_CLIENT_TIME  = 2
};

union set_mode3 switch (bool set_it) {
case TRUE:
    mode3   mode;
default:
    void;
};

union set_uid3 switch (bool set_it) {
case TRUE:
    uid3    uid;
default:
    void;
};

union set_gid3 switch (bool set_it) {
case TRUE:
    gid3    gid;
default:
    void;
};

union set_size3 switch (bool set_it) {
case TRUE:
    size3   size;
default:
    void;
};

union set_atime switch (time_how set_it) {
case SET_TO_CLIENT_TIME:
    nfstime3    atime;
default:
    void;
};

union set_mtime switch (time_how set_it) {
case SET_TO_CLIENT_TIME:
    nfstime3    mtime;
default:
    void;
};

struct sattr3 {
    set_mode3   mode;
    set_uid3    uid;
    set_gid3    gid;
    set_size3   size;
    set_atime   atime;
    set_mtime   mtime;
};

/*
 * Arguments for directory operations (v3)
 */
struct diropargs3 {
    nfs_fh3 dir;        /* directory file handle */
    filename3 name;     /* name (up to NFS_MAXNAMLEN bytes) */
};

/*
 * Arguments to getattr (v3).
 */
struct getattr3args {
    nfs_fh3     object;
};

struct getattr3resok {
    fattr3      obj_attributes;
};

union getattr3res switch (nfsstat3 status) {
case NFS3_OK:
    getattr3resok   resok;
default:
    void;
};

/*
 * Arguments to setattr (v3).
 */
union sattrguard3 switch (bool check) {
case TRUE:
    nfstime3    obj_ctime;
case FALSE:
    void;
};

struct setattr3args {
    nfs_fh3     object;
    sattr3      new_attributes;
    sattrguard3 guard;
};

struct setattr3resok {
    wcc_data    obj_wcc;
};

struct setattr3resfail {
    wcc_data    obj_wcc;
};

union setattr3res switch (nfsstat3 status) {
case NFS3_OK:
    setattr3resok   resok;
default:
    setattr3resfail resfail;
};

/*
 * Arguments to lookup (v3).
 */
struct lookup3args {
    diropargs3  what;
};

struct lookup3resok {
    nfs_fh3     object;
    post_op_attr    obj_attributes;
    post_op_attr    dir_attributes;
};

struct lookup3resfail {
    post_op_attr    dir_attributes;
};

union lookup3res switch (nfsstat3 status) {
case NFS3_OK:
    lookup3resok    resok;
default:
    lookup3resfail  resfail;
};

/*
 * Arguments to access (v3).
 */
const ACCESS3_READ  = 0x0001;
const ACCESS3_LOOKUP    = 0x0002;
const ACCESS3_MODIFY    = 0x0004;
const ACCESS3_EXTEND    = 0x0008;
const ACCESS3_DELETE    = 0x0010;
const ACCESS3_EXECUTE   = 0x0020;

struct access3args {
    nfs_fh3     object;
    uint32      access;
};

struct access3resok {
    post_op_attr    obj_attributes;
    uint32      access;
};

struct access3resfail {
    post_op_attr    obj_attributes;
};

union access3res switch (nfsstat3 status) {
case NFS3_OK:
    access3resok    resok;
default:
    access3resfail  resfail;
};

/*
 * Arguments to readlink (v3).
 */
struct readlink3args {
    nfs_fh3     symlink;
};

struct readlink3resok {
    post_op_attr    symlink_attributes;
    nfspath3    data;
};

struct readlink3resfail {
    post_op_attr    symlink_attributes;
};

union readlink3res switch (nfsstat3 status) {
case NFS3_OK:
    readlink3resok  resok;
default:
    readlink3resfail resfail;
};

/*
 * Arguments to read (v3).
 */
struct read3args {
    nfs_fh3     file;
    offset3     offset;
    count3      count;
};

struct read3resok {
    post_op_attr    file_attributes;
    count3      count;
    bool        eof;
    opaque      data<>;
};

struct read3resfail {
    post_op_attr    file_attributes;
};

/* XXX: solaris 2.6 uses ``nfsstat'' here */
union read3res switch (nfsstat3 status) {
case NFS3_OK:
    read3resok  resok;
default:
    read3resfail    resfail;
};

/*
 * Arguments to write (v3).
 */
enum stable_how {
    UNSTABLE    = 0,
    DATA_SYNC   = 1,
    FILE_SYNC   = 2
};

struct write3args {
    nfs_fh3     file;
    offset3     offset;
    count3      count;
    stable_how  stable;
    opaque      data<>;
};

struct write3resok {
    wcc_data    file_wcc;
    count3      count;
    stable_how  committed;
    writeverf3  verf;
};

struct write3resfail {
    wcc_data    file_wcc;
};

union write3res switch (nfsstat3 status) {
case NFS3_OK:
    write3resok resok;
default:
    write3resfail   resfail;
};

/*
 * Arguments to create (v3).
 */
enum createmode3 {
    UNCHECKED   = 0,
    GUARDED     = 1,
    EXCLUSIVE   = 2
};

union createhow3 switch (createmode3 mode) {
case UNCHECKED:
    sattr3      obj_attributesU;
case GUARDED:
    sattr3      obj_attributesG;
case EXCLUSIVE:
    createverf3 verf;
};

struct create3args {
    diropargs3  where;
    createhow3  how;
};

struct create3resok {
    post_op_fh3 obj;
    post_op_attr    obj_attributes;
    wcc_data    dir_wcc;
};

struct create3resfail {
    wcc_data    dir_wcc;
};

union create3res switch (nfsstat3 status) {
case NFS3_OK:
    create3resok    resok;
default:
    create3resfail  resfail;
};

/*
 * Arguments to mkdir (v3).
 */
struct mkdir3args {
    diropargs3  where;
    sattr3      attributes;
};

struct mkdir3resok {
    post_op_fh3 obj;
    post_op_attr    obj_attributes;
    wcc_data    dir_wcc;
};

struct mkdir3resfail {
    wcc_data    dir_wcc;
};

union mkdir3res switch (nfsstat3 status) {
case NFS3_OK:
    mkdir3resok resok;
default:
    mkdir3resfail   resfail;
};

/*
 * Arguments to symlink (v3).
 */
struct symlinkdata3 {
    sattr3      symlink_attributes;
    nfspath3    symlink_data;
};

struct symlink3args {
    diropargs3  where;
    symlinkdata3    symlink;
};

struct symlink3resok {
    post_op_fh3 obj;
    post_op_attr    obj_attributes;
    wcc_data    dir_wcc;
};

struct symlink3resfail {
    wcc_data    dir_wcc;
};

union symlink3res switch (nfsstat3 status) {
case NFS3_OK:
    symlink3resok   resok;
default:
    symlink3resfail resfail;
};

/*
 * Arguments to mknod (v3).
 */
struct devicedata3 {
    sattr3      dev_attributes;
    specdata3   spec;
};

union mknoddata3 switch (ftype3 type) {
case NF3CHR:
    devicedata3 deviceC;
case NF3BLK:
    devicedata3 deviceB;
case NF3SOCK:
    sattr3      pipe_attributesS;
case NF3FIFO:
    sattr3      pipe_attributesF;
default:
    void;
};

struct mknod3args {
    diropargs3  where;
    mknoddata3  what;
};

struct mknod3resok {
    post_op_fh3 obj;
    post_op_attr    obj_attributes;
    wcc_data    dir_wcc;
};

struct mknod3resfail {
    wcc_data    dir_wcc;
};

union mknod3res switch (nfsstat3 status) {
case NFS3_OK:
    mknod3resok resok;
default:
    mknod3resfail   resfail;
};

/*
 * Arguments to remove (v3).
 */
struct remove3args {
    diropargs3  object;
};

struct remove3resok {
    wcc_data    dir_wcc;
};

struct remove3resfail {
    wcc_data    dir_wcc;
};

union remove3res switch (nfsstat3 status) {
case NFS3_OK:
    remove3resok    resok;
default:
    remove3resfail  resfail;
};

/*
 * Arguments to rmdir (v3).
 */
struct rmdir3args {
    diropargs3  object;
};

struct rmdir3resok {
    wcc_data    dir_wcc;
};

struct rmdir3resfail {
    wcc_data    dir_wcc;
};

union rmdir3res switch (nfsstat3 status) {
case NFS3_OK:
    rmdir3resok resok;
default:
    rmdir3resfail   resfail;
};

/*
 * Arguments to rename (v3).
 */
struct rename3args {
    diropargs3  from;
    diropargs3  to;
};

struct rename3resok {
    wcc_data    fromdir_wcc;
    wcc_data    todir_wcc;
};

struct rename3resfail {
    wcc_data    fromdir_wcc;
    wcc_data    todir_wcc;
};

union rename3res switch (nfsstat3 status) {
case NFS3_OK:
    rename3resok    resok;
default:
    rename3resfail  resfail;
};

/*
 * Arguments to link (v3).
 */
struct link3args {
    nfs_fh3     file;
    diropargs3  link;
};

struct link3resok {
    post_op_attr    file_attributes;
    wcc_data    linkdir_wcc;
};

struct link3resfail {
    post_op_attr    file_attributes;
    wcc_data    linkdir_wcc;
};

union link3res switch (nfsstat3 status) {
case NFS3_OK:
    link3resok  resok;
default:
    link3resfail    resfail;
};

/*
 * Arguments to readdir (v3).
 */
struct readdir3args {
    nfs_fh3     dir;
    cookie3     cookie;
    cookieverf3 cookieverf;
    count3      count;
};

struct entry3 {
    fileid3     fileid;
    filename3   name;
    cookie3     cookie;
    entry3      *nextentry;
};

struct dirlist3 {
    entry3      *entries;
    bool        eof;
};

struct readdir3resok {
    post_op_attr    dir_attributes;
    cookieverf3 cookieverf;
    dirlist3    reply;
};

struct readdir3resfail {
    post_op_attr    dir_attributes;
};

union readdir3res switch (nfsstat3 status) {
case NFS3_OK:
    readdir3resok   resok;
default:
    readdir3resfail resfail;
};

/*
 * Arguments to readdirplus (v3).
 */
struct readdirplus3args {
    nfs_fh3     dir;
    cookie3     cookie;
    cookieverf3 cookieverf;
    count3      dircount;
    count3      maxcount;
};

struct entryplus3 {
    fileid3     fileid;
    filename3   name;
    cookie3     cookie;
    post_op_attr    name_attributes;
    post_op_fh3 name_handle;
    entryplus3  *nextentry;
};

struct dirlistplus3 {
    entryplus3  *entries;
    bool        eof;
};

struct readdirplus3resok {
    post_op_attr    dir_attributes;
    cookieverf3 cookieverf;
    dirlistplus3    reply;
};

struct readdirplus3resfail {
    post_op_attr    dir_attributes;
};

union readdirplus3res switch (nfsstat3 status) {
case NFS3_OK:
    readdirplus3resok   resok;
default:
    readdirplus3resfail resfail;
};

/*
 * Arguments to fsstat (v3).
 */
struct fsstat3args {
    nfs_fh3     fsroot;
};

struct fsstat3resok {
    post_op_attr    obj_attributes;
    size3       tbytes;
    size3       fbytes;
    size3       abytes;
    size3       tfiles;
    size3       ffiles;
    size3       afiles;
    uint32      invarsec;
};

struct fsstat3resfail {
    post_op_attr    obj_attributes;
};

union fsstat3res switch (nfsstat3 status) {
case NFS3_OK:
    fsstat3resok    resok;
default:
    fsstat3resfail  resfail;
};

/*
 * Arguments to fsinfo (v3).
 */
const FSF3_LINK     = 0x0001;
const FSF3_SYMLINK  = 0x0002;
const FSF3_HOMOGENEOUS  = 0x0008;
const FSF3_CANSETTIME   = 0x0010;

struct fsinfo3args {
    nfs_fh3     fsroot;
};

struct fsinfo3resok {
    post_op_attr    obj_attributes;
    uint32      rtmax;
    uint32      rtpref;
    uint32      rtmult;
    uint32      wtmax;
    uint32      wtpref;
    uint32      wtmult;
    uint32      dtpref;
    size3       maxfilesize;
    nfstime3    time_delta;
    uint32      properties;
};

struct fsinfo3resfail {
    post_op_attr    obj_attributes;
};

union fsinfo3res switch (nfsstat3 status) {
case NFS3_OK:
    fsinfo3resok    resok;
default:
    fsinfo3resfail  resfail;
};

/*
 * Arguments to pathconf (v3).
 */
struct pathconf3args {
    nfs_fh3     object;
};

struct pathconf3resok {
    post_op_attr    obj_attributes;
    uint32      linkmax;
    uint32      name_max;
    bool        no_trunc;
    bool        chown_restricted;
    bool        case_insensitive;
    bool        case_preserving;
};

struct pathconf3resfail {
    post_op_attr    obj_attributes;
};

union pathconf3res switch (nfsstat3 status) {
case NFS3_OK:
    pathconf3resok  resok;
default:
    pathconf3resfail    resfail;
};

/*
 * Arguments to commit (v3).
 */
struct commit3args {
    nfs_fh3     file;
    offset3     offset;
    count3      count;
};

struct commit3resok {
    wcc_data    file_wcc;
    writeverf3  verf;
};

struct commit3resfail {
    wcc_data    file_wcc;
};

union commit3res switch (nfsstat3 status) {
case NFS3_OK:
    commit3resok    resok;
default:
    commit3resfail  resfail;
};

program NFS3_PROGRAM {
    version NFS_V3 {
        void
        NFSPROC3_NULL(void)         = 0;

        getattr3res
        NFSPROC3_GETATTR(getattr3args)      = 1;

        setattr3res
        NFSPROC3_SETATTR(setattr3args)      = 2;

        lookup3res
        NFSPROC3_LOOKUP(lookup3args)        = 3;

        access3res
        NFSPROC3_ACCESS(access3args)        = 4;

        readlink3res
        NFSPROC3_READLINK(readlink3args)    = 5;

        read3res
        NFSPROC3_READ(read3args)        = 6;

        write3res
        NFSPROC3_WRITE(write3args)      = 7;

        create3res
        NFSPROC3_CREATE(create3args)        = 8;

        mkdir3res
        NFSPROC3_MKDIR(mkdir3args)      = 9;

        symlink3res
        NFSPROC3_SYMLINK(symlink3args)      = 10;

        mknod3res
        NFSPROC3_MKNOD(mknod3args)      = 11;

        remove3res
        NFSPROC3_REMOVE(remove3args)        = 12;

        rmdir3res
        NFSPROC3_RMDIR(rmdir3args)      = 13;

        rename3res
        NFSPROC3_RENAME(rename3args)        = 14;

        link3res
        NFSPROC3_LINK(link3args)        = 15;

        readdir3res
        NFSPROC3_READDIR(readdir3args)      = 16;

        readdirplus3res
        NFSPROC3_READDIRPLUS(readdirplus3args)  = 17;

        fsstat3res
        NFSPROC3_FSSTAT(fsstat3args)        = 18;

        fsinfo3res
        NFSPROC3_FSINFO(fsinfo3args)        = 19;

        pathconf3res
        NFSPROC3_PATHCONF(pathconf3args)    = 20;

        commit3res
        NFSPROC3_COMMIT(commit3args)        = 21;
    } = 3;
} = 100003;

