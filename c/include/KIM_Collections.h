/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_COLLECTIONS_H_
#define KIM_COLLECTIONS_H_

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.1
 **/
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

/**
 ** \brief Forward declaration.
 **
 ** \since 2.1
 **/
#ifndef KIM_COLLECTION_DEFINED_
#define KIM_COLLECTION_DEFINED_
typedef struct KIM_Collection KIM_Collection;
#endif

/**
 ** \brief Forward declaration.
 **
 ** \since 2.1
 **/
#ifndef KIM_COLLECTION_ITEM_TYPE_DEFINED_
#define KIM_COLLECTION_ITEM_TYPE_DEFINED_
typedef struct KIM_CollectionItemType KIM_CollectionItemType;
#endif

#ifndef KIM_COLLECTIONS_DEFINED_
#define KIM_COLLECTIONS_DEFINED_
/**
 ** \brief \copybrief KIM::Collections
 **
 ** \copydetails KIM::Collections
 **
 ** \sa KIM::Collections, kim_collections_module::kim_collections_handle_type
 **
 ** \since 2.1
 **/
typedef struct KIM_Collections KIM_Collections;
#endif

/**
 ** \brief \copybrief KIM::Collections::Create
 **
 ** \sa KIM::Collections::Create,
 ** kim_collections_module::kim_collections_create
 **
 ** \since 2.1
 **/
int KIM_Collections_Create(KIM_Collections ** const collections);

/**
 ** \brief \copybrief KIM::Collections::Destroy
 **
 ** \sa KIM::Collections::Destroy,
 ** kim_collections_module::kim_collections_destroy
 **
 ** \since 2.1
 **/
void KIM_Collections_Destroy(KIM_Collections ** const collections);

/**
 ** \brief \copybrief KIM::Collections::GetItemType
 **
 ** \sa KIM::Collections::GetItemType,
 ** kim_collections_module::kim_get_item_type
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemType(KIM_Collections * const collections,
                                char const * const itemName,
                                KIM_CollectionItemType * const itemType);

/**
 ** \brief \copybrief KIM::Collections::GetItemLibraryFileNameAndCollection
 **
 ** \sa KIM::Collections::GetItemLibraryFileNameAndCollection,
 ** kim_collections_module::kim_get_item_library_file_name_and_collection
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemLibraryFileNameAndCollection(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    char const ** const fileName,
    KIM_Collection * const collection);

/**
 ** \brief \copybrief KIM::Collections::CacheListOfItemMetadataFiles
 **
 ** \sa KIM::Collections::CacheListOfItemMetadataFiles,
 ** kim_collections_module::kim_cache_list_of_item_metadata_files
 **
 ** \since 2.1
 **/
int KIM_Collections_CacheListOfItemMetadataFiles(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    int * const extent);

/**
 ** \brief \copybrief KIM::Collections::GetItemMetadataFile
 **
 ** \sa KIM::Collections::GetItemMetadataFile,
 ** kim_collections_module::kim_get_item_metadata_file
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemMetadataFile(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString);

/**
 ** \brief \copybrief KIM::Collections::CacheListOfItemNamesByType
 **
 ** \sa KIM::Collections::CacheListOfItemNamesByType,
 ** kim_collections_module::kim_cache_list_of_item_names_by_type
 **
 ** \since 2.1
 **/
int KIM_Collections_CacheListOfItemNamesByType(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    int * const extent);

/**
 ** \brief \copybrief KIM::Collections::GetItemNameByType
 **
 ** \sa KIM::Collections::GetItemNameByType,
 ** kim_collections_module::kim_get_item_name_by_type
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemNameByType(KIM_Collections * const collections,
                                      int const index,
                                      char const ** const itemName);

/**
 ** \brief \copybrief KIM::Collections::CacheListOfItemNamesByCollectionAndType
 **
 ** \sa KIM::Collections::CacheListOfItemNamesByCollectionAndType,
 ** kim_collections_module::kim_cache_list_of_item_names_by_collection_and_type
 **
 ** \since 2.1
 **/
int KIM_Collections_CacheListOfItemNamesByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    int * const extent);

/**
 ** \brief \copybrief KIM::Collections::GetItemNameByCollectionAndType
 **
 ** \sa KIM::Collections::GetItemNameByCollectionAndType,
 ** kim_collections_module::kim_get_item_name_by_collection_and_type
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemNameByCollectionAndType(
    KIM_Collections * const collections,
    int const index,
    char const ** const itemName);

/**
 ** \brief \copybrief <!--
 ** -->KIM::Collections::GetItemLibraryFileNameByCollectionAndType
 **
 ** \sa KIM::Collections::GetItemLibraryFileNameByCollectionAndType,
 ** kim_collections_module::<!---
 ** ->kim_get_item_library_file_name_by_collection_and_type
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemLibraryFileNameByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    char const ** const fileName);

/**
 ** \brief \copybrief <!--
 ** -->KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType
 **
 ** \sa KIM::Collections::CacheListOfItemMetadataFilesByCollectionAndType,
 ** kim_collections_module::<!--
 ** -->kim_cache_list_of_metadata_files_by_collection_and_type
 **
 ** \since 2.1
 **/
int KIM_Collections_CacheListOfItemMetadataFilesByCollectionAndType(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    char const * const itemName,
    int * const extent);

/**
 ** \brief \copybrief KIM::Collections::GetItemMetadataFileByCollectionAndType
 **
 ** \sa KIM::Collections::GetItemMetadataFileByCollectionAndType,
 ** kim_collections_module::kim_get_item_metadata_file_by_collection_and_type
 **
 ** \since 2.1
 **/
int KIM_Collections_GetItemMetadataFileByCollectionAndType(
    KIM_Collections * const collections,
    int const index,
    char const ** const fileName,
    unsigned int * const fileLength,
    unsigned char const ** const fileRawData,
    int * const availableAsString,
    char const ** const fileString);

/**
 ** \brief \copybrief KIM::Collections::GetProjectNameAndSemVer
 **
 ** \sa KIM::Collections::GetProjectNameAndSemVer,
 ** kim_collections_module::kim_get_project_name_and_sem_ver
 **
 ** \since 2.1
 **/
void KIM_Collections_GetProjectNameAndSemVer(
    KIM_Collections * const collections,
    char const ** const projectName,
    char const ** const semVer);

/**
 ** \brief \copybrief KIM::Collections::GetEnvironmentVariableName
 **
 ** \sa KIM::Collections::GetEnvironmentVariableName,
 ** kim_collections_module::kim_get_environment_variable_name
 **
 ** \since 2.1
 **/
int KIM_Collections_GetEnvironmentVariableName(
    KIM_Collections * const collections,
    KIM_CollectionItemType const itemType,
    char const ** const name);

/**
 ** \brief \copybrief KIM::Collections::GetConfigruationFileEnvironmentVariable
 **
 ** \sa KIM::Collections::GetConfigurationFileEnvironmentVariable,
 ** kim_collections_module::kim_get_configuration_file_environment_variable
 **
 ** \since 2.1
 **/
void KIM_Collections_GetConfigurationFileEnvironmentVariable(
    KIM_Collections * const collections,
    char const ** const name,
    char const ** const value);

/**
 ** \brief \copybrief KIM::Collections::GetConfigurationFileName
 **
 ** \sa KIM::Collections::GetConfigurationFileName,
 ** kim_collections_module::kim_get_configuration_file_name
 **
 ** \since 2.1
 **/
void KIM_Collections_GetConfigurationFileName(
    KIM_Collections * const collections, char const ** const fileName);

/**
 ** \brief \copybrief KIM::Collections::CacheListOfDirectoryNames
 **
 ** \sa KIM::Collections::CacheListOfDirectoryNames,
 ** kim_collections_module::kim_cache_list_of_directory_names
 **
 ** \since 2.1
 **/
int KIM_Collections_CacheListOfDirectoryNames(
    KIM_Collections * const collections,
    KIM_Collection const collection,
    KIM_CollectionItemType const itemType,
    int * const extent);

/**
 ** \brief \copybrief KIM::Collections::GetDirectoryName
 **
 ** \sa KIM::Collections::GetDirectoryName,
 ** kim_collections_module::kim_get_directory_name
 **
 ** \since 2.1
 **/
int KIM_Collections_GetDirectoryName(KIM_Collections * const collections,
                                     int const index,
                                     char const ** const directoryName);

/**
 ** \brief \copybrief KIM::Collections::SetLogID
 **
 ** \sa KIM::Collections::SetLogID, kim_collections_module::kim_set_log_id
 **
 ** \since 2.1
 **/
void KIM_Collections_SetLogID(KIM_Collections * const collections,
                              char const * const logID);

/**
 ** \brief \copybrief KIM::Collections::PushLogVerbosity
 **
 ** \sa KIM::Collections::PushLogVerbosity,
 *kim_collections_module::kim_push_log_verbosity
 **
 ** \since 2.1
 **/
void KIM_Collections_PushLogVerbosity(KIM_Collections * const collections,
                                      KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::Collections::PopLogVerbosity
 **
 ** \sa KIM::Collections::PopLogVerbosity,
 *kim_collections_module::kim_pop_log_verbosity
 **
 ** \since 2.1
 **/
void KIM_Collections_PopLogVerbosity(KIM_Collections * const collections);

#endif /* KIM_COLLECTIONS_H_ */
