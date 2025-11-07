#!/bin/bash
# ------------------------------------------------------------
# find_id_serializers.sh
#
# Description:
#   Locate Template Haskell macros related to ID serialization
#   in Haskell source files and extract information about the
#   types or modules that use them.
#
# Usage:
#   ./find_id_serializers.sh --types     # Print all [t| ... |] type constructs
#   ./find_id_serializers.sh --modules   # Print modules containing derive macros
#
# ------------------------------------------------------------
set -euo pipefail

SCRIPT_NAME="find_id_serializers.sh"

print_error() {
    # Print error messages to stderr with script name prefix
    printf "%s:\t%b\n" "$SCRIPT_NAME" "$*" >&2
}

find_id_macros() {
    # Search for Template Haskell derive macros related to ID serialization
    #
    # Arguments:
    #   --macros : Print matched macro lines (default mode)
    #   --files  : Print only file paths containing those macros
    local mode="$1"

    case "$mode" in
        --macros|--files) ;;
        *) print_error "Invalid option: $mode"; exit 1 ;;
    esac

    local macros=(
        deriveIDSerializer
        deriveIDSerializerWith
        deriveIDSerializable
        deriveIDSerializableWith
    )

    local script_dir
    script_dir="$(dirname "$0")"

    for macro in "${macros[@]}"; do
        "$script_dir/find_th_macros.pl" "$mode" "$macro" src
    done
}

extract_type_signatures() {
    # Extract type quotations of the form [t| ... |]
    # from the given input (likely TH splice code).
    grep -Poz '\[t\|(.|\n)*?\|\]' | tr '\0' '\n'
}

convert_paths_to_modules() {
    # Convert file paths to Haskell module names
    # Example:
    #   src/Foo/Bar/Baz.hs -> Foo.Bar.Baz
    sed -e 's|^src/||' -e 's|\.hs$||' | tr '/' '.'
}

main() {
    if [[ $# -eq 0 ]]; then
        print_error "Usage: $SCRIPT_NAME --types | --modules"
        exit 1
    fi

    case "$1" in
        --types)
            find_id_macros --macros | extract_type_signatures | sort -u
            ;;
        --modules)
            find_id_macros --files | convert_paths_to_modules | sort -u
            ;;
        *)
            print_error "Unknown option: $1"
            exit 1
            ;;
    esac
}

main "$@"
