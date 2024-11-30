// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterFlow",
    products: [
        .library(name: "TreeSitterFlow", targets: ["TreeSitterFlow"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterFlow",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterFlowTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterFlow",
            ],
            path: "bindings/swift/TreeSitterFlowTests"
        )
    ],
    cLanguageStandard: .c11
)
