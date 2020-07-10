#include <archive.h>
#include <archive_entry.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define E(func, ...) \
    do { \
        if (func(__VA_ARGS__) != 0) { \
            printf("%s error (%s:%d)\n", #func, __FILE__, __LINE__); \
            return 1; \
        } \
    } while(0)

void print_usage();

int main(int argc, char *argv[]) {
    if (argc <= 2) {
        print_usage();
        return 1;
    }
    const char *path = argv[2];
    if (strcmp(argv[1], "write") == 0) {
        if (argc != 5) {
            print_usage();
            return 1;
        }
        long long num_files = strtoll(argv[3], NULL, 10);
        long long file_size = strtoll(argv[4], NULL, 10);
        if (!(num_files > 0 && file_size > 0)) {
            printf("Invalid write arguments.\n");
            print_usage();
            return 1;
        }
        struct stat buffer;
        if (stat(path, &buffer) == 0) {
            printf("File already exists at %s\n", path);
            return 1;
        }

        struct archive *a = archive_write_new();
        archive_write_set_format_pax_restricted(a);
        archive_write_open_filename(a, path);
        char *file_buff = calloc(file_size, 1);

        for (long long i = 0; i < num_files; i++) {
            char filename[50];
            int n = sprintf(filename, "archive/file%lld", i);
            if (n < 0) {
                printf("sprintf error\n");
                return 1;
            }

            struct archive_entry *entry = archive_entry_new();
            archive_entry_set_pathname(entry, filename);
            archive_entry_set_size(entry, file_size);
            archive_entry_set_filetype(entry, AE_IFREG);
            archive_entry_set_perm(entry, 0644);
            archive_write_header(a, entry);
            if (archive_write_data(a, file_buff, file_size) != file_size) {
                printf("unexpected number of bytes written\n");
                return -1;
            }
            archive_entry_free(entry);
        }
        free(file_buff);
        archive_write_close(a);
        archive_write_free(a);
    } else if (strcmp(argv[1], "read") == 0) {
        if (argc != 3) {
            print_usage();
            return 1;
        }

        int64_t total_filesize = 0;
        long long num_files = 0;
        struct archive *a = archive_read_new();
        archive_read_support_filter_all(a);
        archive_read_support_format_all(a);
        int block_size = 40960;
        char *buf = malloc(block_size);
        E(archive_read_open_filename, a, path, block_size);
        struct archive_entry *entry;
        while (archive_read_next_header(a, &entry) == ARCHIVE_OK) {
            num_files++;
            int64_t entry_sz = -1;
            if (archive_entry_size_is_set(entry)) {
                entry_sz = archive_entry_size(entry);
            }
            int64_t read_sz = 0;
            while (1) {
                int64_t block_read_sz = archive_read_data(a, buf, block_size);
                if (block_read_sz < 0) {
                    printf("archive_read_data error\n");
                    return 1;
                } else if (block_read_sz == 0) {
                    break;
                } else {
                    read_sz += block_read_sz;
                }
            }
            if (entry_sz < 0 || entry_sz != read_sz) {
                printf("unexpected file size in archive\n");
                return 1;
            } else {
                total_filesize += entry_sz;
            }
        }
        E(archive_read_free, a);
        printf("Total filesize: %lld\n", total_filesize);
        printf("File count:     %lld\n", num_files);
    } else {
        print_usage();
        return 1;
    }

    return 0;
}

void print_usage() {
    printf("bench-archive write [path] [# files] [file size]\n");
    printf("bench-archive read [path]\n");
}
