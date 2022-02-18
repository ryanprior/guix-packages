import {Packages, Records, BuildSystem, GitDownload, Licenses} from "@gnu/guix"

const {"node-xyz": {"node-debug": nodeDebug}} = Packages
const {"node-build-system": nodeBuildSystem, BuildPhase} = BuildSystem
const {Package, Origin, GitReference} = Records
const {expat} = Licenses

const version="1.0.1"
export default Package({
  name: "balanced-match",
  version,
  source: Origin({
    method: GitDownload.fetch,
    uri: GitReference({
      url: "https://github.com/juliangruber/balanced-match",
      commit: `v${version}`
    }),
    sha256: "0bag2pynqh5lz82l3l6478arwza95xwdjshfchlz3sf2f8sdyiv8"
  }),
  buildSystem: nodeBuildSystem,
  arguments: {
    tests: false,
    phases: [
      BuildPhase({
        name: "delete-dependencies",
        after: "patch-dependencies"
      }, buildContext => buildContext.deleteDependencies([
        "tape", "standard", "prettier-standard", "np", "@c4312/matcha"
      ]))
    ]
  },
  inputs: [nodeDebug],
  synopsis: "Library to match balanced character pairs, like `{` and `}`",
  description: `This library provides an API to match balanced string pairs, like { and } or
<b> and </b>. Supports regular expressions as well!`,
  license: expat,
  homePage: "https://github.com/juliangruber/balanced-match"
})
