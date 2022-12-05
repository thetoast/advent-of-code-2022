module Day3Input where

testInput :: String
testInput =
  """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

realInput :: String
realInput =
  """gvNbShZZgQfWdQhdPQmggLTFLwmwjFqjVVgM
CsJnHllcsnnnnJrGRnRwPPLVmFLHLBjFFVHmPT
JlnCtctJnJDcJlDCRpPrSSQWfphzWZfbZSvfNfbS
WjvRSdSQjvpjWzNlnZlNZqCCMzZZ
nJtJsbctPBPwLNcDZNNGLClC
tsFJHBgJwgJbnvSHHWVWHhVhpQ
zRzPhCCSHVZzfGHZ
qBsWBpqBwBcvqqWgdfZrprdggPHHVZ
WWmvwvBbnWmnwvWcbmWWnqNCRSDRRSSjjSDbPJbRjClLhC
rQrznfHHhrHzllzlzTGcJgtJ
jhhjjSZVPWVZvSFtTttLTglgFtvm
dqSCqVWdbDSSVqbVVSqhNdrnpnCnfsnnwfnsRpMpBMrf
rTnvrSSHvHtnDQVDtfmW
ZjCglhcCJschpZbZgbtGmRtbGmwtQtbmtN
FFZCghFJhlslFpjcdFTPLHHmrqTMTdzBLHLH
WzDfrDwwDCCDMnfDHHJjTHTtNdngdHtQ
FmScGPPPPmpspchbGHtRsjdJJJWJWNttgH
bvhFlVhPchPvqWLwqLWqvq
RcchVlCCvmhDRjNJJJjbjllZlJ
wPwFGFMPfpdtqbpwFFfGPQZTWTZjtzQWTBBQJzZjZZ
fbwPqqMPwdgvmDVDhmRCgR
SSQggtQVQQgQGmGVthVnWDGjCBLLzdDNCCjDGWCW
RcHcHbFqbPMZRFTvHFFFlcZZBNBtNjjLLjDdjLjDqWCpNLpj
sPPclZcsZtMPfVsnnwwVJhmQ
jlrwpVPjMwfzZfhfwddH
DbQBGgBJPPBBHfPZ
ggGSQTnTgQGbSRQRPrNlsjVjMrpmMLnrrW
MzNNbMSrZNSSvGWWznwGQlTG
ZCFCJHqJFjqTTqngPwPG
LCJLJCCFtfjsJCMbZSMSbbcDdBfd
LCdjljfJJBfLDCCdJqGqsGGtmBsWtbGmSS
gpgchRcPgRRQNZcpNhgNPzzmGrmrGwtSSswqrSGbbTzt
QgQvNppRPQcggZNccchvNZgCjlnsDLDflDdfjLfMDVsMLv
jfSfTWfwTJffQQNwTCHnGGGgHbgqGFvF
LmspctdsLDlBLPmFFgqbqFFbqlnSRS
PSZZsctsDDtdBmzBLBSmpJjMzWwjVfwJVMrNfwrfzf
zFZqbRNRHNHhqHFqzNnzzqDQVFMggMgDpmJMTgpVTQFJ
SwLVsSWBfStDGMfDTDGGGT
vlCBVsSvCRrHRjCqrn
ndGVddlVdrcGlGcljdVGjCgfffPCCLfCMvqWvPnqgn
FNtRZBDDSNSNStDhFRNQtLgCMTCBqvLMMTfJqBLPLP
bwQwZhSDFhZbFSNccwzwHmHHjHMwsd
BrSDgqrgWzWDwJBzMDWBLjpmVLFfFPLVJjVVPLFV
cNnRlvNRvQTcnZwmfQGPmfVFPP
dsCNllvTdnntTlTsHcdCRtNRBDWBDWDrWBhDBqMBDqbzWwCr
stLcchcTwRcsVTtftVVthchqmmCCFvWmmgSgQFCwGvWSFCrr
PljDbZbzjllHdQNWHMFGrMMHMFrmmM
njjjzbPdqRcRQQns
BTBLwzmqWNbpzqNzLppLBnhZJcJSHThhnnJZHHSQnr
RfQgDjgsjVfRFDdvddtvhhhJCnrSnMZCcMSCSJ
QRtjtRQQlzlqqzzzBW
FnzllplJMntnzptLFzsjTVSQcSjSjQFFmdQQ
wgwrBrwGHPGBqHrDcpmjddSQTmTGVmmG
DBhhNBrCrMvNvzptpN
LgSbLLRJQSzLCgJRJhWCCzRBdZdcMfcHsMdFFjfPjsCHBc
nrVTnmrrrlnGGmrlVwVZPFHmdsZHBmjMcdjfcZ
wrnTlNpvVVrqFrtnwGqwwrTpSgSQQLbQRzQSQRJbQpDbQz
pgvmcpVcpwJppwgwvMWfHbRGRHDBJbWGfGGb
hQqFqQNhqdCPNhTQCCFbbjmBHBbBRjGHBDBGFj
TtdCCNlPCmssnqgvzzvvtVrgzzrc
JltddTSgtLLvgvdldgvWPZPPSjWMjCfWNjNWPG
FrwpbwhbmfjGRpGPNM
cmBrBcBnmFBQqcFbscBhQfttznJgzlLLDlnglzLLlg
mDhRsDzWZzGhhhWpZwzwGdfbFsfFTQTLcNFfFrNvvcbT
ngMHjjgVgqHnPlnqtnClMHbfCJFbcLFFJFLbrQNbFQcQ
SlqHqVVqPnBHSHHljlMndBdZQphRhdGpzWmWWwBh
hVNhcfbSSNMfjjMlqRCqpdzpRJdnhl
sBHPPwQwttBBssLCHzDBlDDmRmpJdqJJJd
sHHwtsHTWwtZPHtZbbVSgMbGCNjNvN
SgMtSMVChFBHBFdCwhdqPcPsbRVQqsblDbRmlc
jlrLWfvWvzWWzmcQcqcNbmNDjR
JpLznrnfGrrfnWWzTpfvzvdHHCgSBwdlTMhCTMFhBBwg
fptnFPLsttddDdRhQShQzC
BqGHGlqmmGmlJmNmqlbbCzSCQHDzSjHhfSShjfVh
fbrrMqGGBlNMGNqrqglmrLZwFcwgFwsPcscctpWLgn
TCMMTtWBqCMMMWWMSWhNFhlNBNlNHwNDsvhw
frfdrbZJrVLdZLbnJcrmvhlhHshlgsZsHwwghZ
fLRVvcvRVpfbcfVVJTqpWCCSqCCTTtjjPj
vRCGzdTtvdBCTzCdmVsQjnVttngthgnQHs
rLNZlrwrjrFLMsggsQpsQngqMq
NfjZWcbffDrLZfNLbDczvmcTvvGCmCBBmSdP
DSqdzrHgJtSHMgvmnNGdcmCnvssN
ZwQLlVlRVVWZFZhbJZVjRJscvBGcpssLpBcnGpBsmpBs
TwwjjQJTjlhwMfTDgMrfgtqg
HhvVhgddvdvqTqVqHQgjgmCPWGrcWsGWPcvlsWrWmC
lbJBBFbMlJFRCmsRmRPtCtwm
FMLLpDLnFpFJbDfLnZZSzhqHVdVQjgSdlQTZ
mvBrszzsrrrGsBDvBvszDRQDhjCwWdLhJdCjjWZdqqdZdW
gMlfFVccTGMGPMSCdcLLdqhjLJWWdZ
VfggggPVltVSlSlHfgFtTpfRGmRrrrvQpQsrGmnvnQbzmz
HjtMgWbHBtbtcggVcHwMwcdzpZFGzGZFpvFFZZWnnpFG
RNNrfmSrSSzdGBzFfJFq
mNsSrrTPPRshmsBClRClPmSccVgQVHjHwtHwTMbTVwMHgg
pZmmLpQLDJBCPCssJTsCTc
SwlLwWvSwNSSSMSMfWLvvVdPFzjfTscdjcCsjzCPddFc
vbVMqGbLSVMhDQnRRQnBqH
lNmNwlmlbhhfFNgpJLnHfTHfTdnTML
BGvCwVGVPSDWDMMJTHZMpndH
rVcBrPSPCwWqPwGCBCSrqlNNQFhsQhtlhhhgNQtb
HqZCQRQgWjpmZHRHqQjttGGJQNQtwQGGGbGrJz
SlddfFVMLncbJJMzMzzMwD
ndSznsdVHsqgpZvH
sNStMtNtDztrvrRGPRRRRrNzqcchgJJLqnBScBcJThJggJBT
QCWWZbCbCVjVbFVFbVwbnhqLqhghBLwwTqJJcpgTBL
ZlHjfHHFWFjCdbHlQFnfdnZFMzPzrrlMrtrrGRMsDPrGvGPz
NnDHhNNldhNTdHllpptCRtftzmGBGmVRVGRH
wgZgZJLBwqgLSWrBvZVVzmffWzRmGsGmmssm
bgqPBBLbPbrrZBjjlpjccjDdDchFTNlc
WRWNNRWNfLZtsmtSWrtLmWgTcMcPrpMTcVMMPPBBqjcg
QJJDFbpnpJdlcwcgBTqlgPwc
GJFHJGhHnpWWShpsRS
DHDZvDQWdHJMHlJf
RhhnGLnBtGrnLjwwJTTMTdQccllSMQ
rmFrrBGQLQFLjbFnBhQLhrRGCPVDWzgmpsWvWWDzppWpzCvC
bsrJrWgWJvQRqbbRDNGGZDlwNfNfwS
ttCHThhhcBVpTBcdhdpfNGDSQLwDLfDGfQllCG
jFjzcFhzQTTjHFvJsrPPnnFgPb
SZggDgNLGCQzSgRsljscPvPPbHsBNc
TtFfTpMthzhFFTpdMMJPfjPPWjBbHbjPlPPBjj
dpdttMMTJttzMFhqFwGLgGRSrrQGSZrwRrDQ
JsJntWmghjgJTpfgFCczlztzrDZDtDzC
QddBBdBSPPPHBSVHVHvNdPVRClvlcclCDLCLFRFrrDLcZc
NVwBVSNlSBNHngnngsJWhwWh
hQVWJGWJlJQDGJHQWHpVWJVHmfPGPcjfdLdLSrcCmfjPjCPm
wgwnRwMvNvBvFZMngNvFZCjdSSfcbPLjCbcdCbSf
wgnRvMMvjMsTwngTgnnDVDWJDplHWhlhQWlWDT
WGswWFGsBFwHvjnbnnJjbG
gLDrSrPpLfmmDqqbbJZsjvmvRv
DTldTpTsDLgSsLpfPTslVdVVdNWVzcNwtWdWhzVt
sttWrhWZsLVtJVZrWhgbCPQQbDNVCHglnnlN
vvRRRMqpmTjwFqmfjFjmBMwNbCwQNgHgDnbNnHQHQCDg
THpvmBGfzWGzSzzS
nmZwwfPmNggwgPgNmdvPPhTRHTHBJTsvSP
WDccMzVQzGWcrllLdsGTvTsJJBLTdL
pWtzQrzDlrCpcQztlzCCCpCVFFFwtgmfbjmbqwZjZfmFwfdw
bmMNNMVSRWBWCSmVRdMRmMnvZZscvvszGcnZClpcGsvZ
gfLfDDtDgjgDtWTjFQjHsvvpZqqplfvvcllcnnvG
TFPjDtWPNPwBBBNB
QcTCHcTwdpQgcWzWwvMPRzSGwW
sDNjfhVmnlVffmLNfLbMPPDbqGtMSRWtDMtP
fZjJJshhhLNnrCJScHSccTQQ
qfhBhLLjvMqWWBWjsQGrdrGzpszpGrqd
JTSDHFtlTDbHTcFcDbTHzmrNRdRRbNzrmRRnRpnQ
PHcltTgJDlplgptScCfCgWMVfvfZvhMMBV
rttDPglHZZDDDPHgZtgWBswdsTfpfdplLqfsFfFdfd
cRVvpcmpmRSCGGCcMQmQFqNNqdVdqTbLwNbdFfFs
zhGjRhSvcMtnrpZjHZBg
STNrrzVdTJwsBggwFgBN
WbtMWJJpwpDsLBfP
cbvncCCmWtCGMcZHVVSlJVQrzvzVqQ
MtPVBHVbBBTwbvWgRgvF
LhZjZpnnMRwWfhSh
LQcQCLCCCcnmnqZpcrpnrjQlsBMHddtMGGzrJHPPsHsNPPJz
NbZtmZDmNNDBHBhDNtNGcvpmgLmmFrvprgrFGv
djMjffMdqnVVTdMfTPnQSFScFFVSLLFrvccvcllL
PPdPwMQwTMWDwBDZLHZN
DqpJFdhtmdSwpqSncljHsnfjqlbzlc
rQVZLGMWPCNrCGMwNVMcbzsVgfHHgfncfscssH
NBCrPLwMTrrMBLQLwCFhpFBDFdFFtdvDFvpD
QRwGBdGwqRTBMWRMMzBqQHHhngspPFFbHbzgbhZLLP
rCNmDjvrCFpvLZsgPb
lDrmpcfCtVDtCjlfNfQQqVdJBQWRVVRMMMJQ
zcgjlRcJCFtlnTvppn
BLSHDMVPVPHVHwFwMhTvgnFZMh
DsLSDdmPVNVdGgrzCCdbjc
tjjRbNQtvJVVsvqsvdTsGqBp
wlFLPZPzzzrzwZPmZfZPlrmpLDCpWDWCSTCpppDDBssdWG
rwwcFFghFmgTcNRJNnnNJtHV
RLhbzMhccqLzdjghdFddNWPN
DmsTZfvrfJLPPWwrdFgw
LsmDtQDfLLGbGQbSzcpM
qwhmmHlHmlwChcCWlpPCBzjPVdBzLsdLBVsT
DnJDMqRDsPsjRVQR
tSGSnNNSbrJJGfDSSfbchpvptFwmpcvlHqZlcF
rdQdBRPrQBBnfdBbzLLgLgmbzSLCPW
TcRTcFDTcjvswMWzzgLSzwwSLJbL
MNTRMNGTsFMMqqqcMDTTnfpBhQhNpHZBZHBfrptQ
ZZFwcQGwRGLTGLTl
rJtMrqBVvhBCnlCn
WWWMttbqqzfPWqWJVzmDQwFQlwZwjQfjgFcZ
RhgmdBbLLmRvhGZwhZZJDwGq
ldPpTtTPtVPpfTGzJQFGFqJsFG
MPpCnrlCVfPtrPVtntllnbdHSbnBvvgdgHgnWvSv
nbnfjQbQZBqBvjQdVFGHhsQhsFsFzGpD
RSJSTTcCVTTMlSNcSTRRwTccpwDFDDzFhHhtwppssFGGpsDp
clcTNJlLMgrcgRvBjfWVfWfBrrZn
LHQdpQLQDRcDBQccVQpnNRhllzMtRqzRztTNnt
mrvZPZvZvZbPSFJrSrggFvrJzbhlzttTlNtqtsMtTntntdqq
GPfJPJwfZFrmFvrPmgHHQLQWdHQBwWWCHVVC
TvTTLfflhZmwZLvtJmhdFhMpnWrpnbJcWMnpHzMrzbcn
VqRRNGCBjRmqPGqDzbHMcMccWnzngHnP
jNqNBjsDsDRCNVsmBRNQNRSSSddhZvLhddZhvhwvLTZwhSfT
vjpJvpgpNwjDWvvJLhFLLhNRTmPTRPRF
HHbHVrMHMmqzHlGmtGqMtdLCLQthQQPCSPTSFLLPRT
bZZzbHszMrqzGmrHMBzbHbqJjffcDgwWjDJpnjjcscwgnw
fBHDMNhhHMrBDBrfQqfwwvvLvfjLZZ
lmppmGJgPPpddJVdnwsQjLttqtjZTpcsvT
RndRmlGWJgdRnWQJQgWdQPlFFbhhbFbHbHMhhBbRDRFrrS
tVhwlMdMWlhlZMZMlzWrtRpBNpLPpmnmCPQBLppPPPBz
GvGTsgjcTLRQHmTQ
gGRGbfbgbcSbbcRggvfcwlZVhVMfltddVtrlZdhW
PwjPHwWssqrHqCqprpCPjwrrQnnDbdnQnGQHnbZbGQZbBQdD
WvNtLhfLndnBNbJJ
fvhvvWfTmtzTmjpsVVqsPzMjjq
GjHMMTMfZbnqHnJNCN
rlpQpthdlQbnlJqRzqVL
spphwcpBvcvdvFTqPDFfwwPZTG
mQfqmtpfBHJCHdlMDB
cbbPPsZqjqcSvVsCrDllsrdzrzzD
TSbVTvShqnVjPTPbggSPbNFQWGtfNLtFfWgGmGgGWR
HJtwgJJwJrcjRRjRnwhVdrVbmGhPmNNrGhdV
fWFvTDssTDMCBCWsvszWWMCQhZhLLNvNNbhVGGZdRZRdmhmd
zCTBzWzDDCDCfFCRqCJHcJJctjJSgHpqHqJg
dgnwgbcwVGbgDWnQbjdgDnBRBBNNqRLBNMLFRMRlqNsc
fTZHmTmzJMmSSCZChZvZTHRlBNBFqtqftlqssqNqftts
JTHJvhzCHpmPCSSTzPpPzJHgQVdQQjQdQwjgdpgdDMGgQM
qPGmWLJWBRRdvqvvNq
TnZHsTZsZsZZbDtbrNrdnDft
SShhQQZCHQCSTVlllVVQCjQWNJGFWpWgFpmmVmNgLgFpGw
TsSmttWMVstNfbMfBcpnFchFwFwZNrrwFp
DWWCGlHWdhddwpcrjh
gQvPqglRCHCHCPCPCJbbtSVVWtTVVJvTbs
ddHSZQHDStZStLtsdDpbCbBPcjGGCqCMBGdNPb
hWhvmglVznzglgzvzfznwfnfscswPjbMBMjNGBMPGMcjcPwP
sJWsJfVflWsnhrltrZLFDDRRFTSSLT
qqCqLNjnPPLZPjqnDjLNgnNrcGwtgddGrhprBJhhJtBGJG
lTMDsDHmlRRbQQbVfMHFmMBJhwcTddrcpBBGdGGJcJdB
lHFfQFssFFFMQRVWQVlssLnDSLLPvZZPqWCLWSqjvS
pJPZRpQtpPQpGPqWbqlWTdLVLJbT
nnnjCwgrgcCFBfnggBCsMTVbVsWVWdCTTdbb
cfhcnwnrcggrncrwcjBDcGHmNPRRGRZZQRRdDZzmzH
NqTrrzLpTNdLLJBBcBGGZGpFHB
sWbgfHSfSgVgZcmGtStZwBFG
VjQjjPjhbbfsCbbDQCgsDrRhzMdlNzHTRdRhqdNTrq
GlqfPdvdBqPgfqDgFMsMVDppbsNJNpMs
WnmmWCTCjZnmzRjTZRRnFBMNpbZZpFJMSFSNFSbS
TRRrnLBmzjttRRCmTRjjwLlvdhQvdrhqqlGdPqGggPlQ
McjMPgPgGPHJWjhPNgPqQbpQSfqffnWVnnnpnZ
BwwDmBTLRBBLzBTBTvtVqlfpqfbQqpqZbnQFlt
rDsDBsmvRTBRzCTZsTmCsCrLPgNcPHNhPccNHHrjMNrgdNdP
GqmLFDrcglvQGZwwzj
nlHbSNBhhslJfsBfvdZHdCvRzdCzppzR
JWShMfWMMthSJBBthJsgtcTTcFqcTrmTLrrTlr
CsBsShBWsBWhvFJWCSsJpbTZdTbdBRgHffRlHHHBgH
tqmnwwwDmVwfzZblzTRqzQ
DGtDjGMcntMGnnGhhjJJJsCpFRRSJJ
GQVVcGgFGcSFvfcFfJVnQmdbTwMLwTTnbnQMws
ZqZrDWDtNPPHRNPzGCdndLLMLwMWhwbmLddn
DRqRHCHlCtDrqtNqDplfpvGFjVjvBSFFcj
zfSfdGPdMVHHdcMThhpwqqrwrqJhcF
vlDNTWDDWWnngNQZtgwLrJZqLFZLFJpFJjJJ
NCtWmnvBmDnWQVTmTGSRmmRTPz
LLvppVLDsppGMGCLCCwNmJMNrNHrNmNNmHFjrh
lZZfvfgRPZQWNNmlWlFNHl
nnRtndRbcPttvTpswLTs
trtzWfszNPlhPlgrWglhZjSLDBvTfSDTmSvmLvfS
QMdVVGBQMTJDvSjdHJ
nBppGVVnQQgzgPsPzzhp
fVQzVrQVtVzHwjtMTjcTCjFgcDCGCd
JslDPWLbLbZlJBMgBTBPdcFTFd
JNJmZsDLnLDWmsVqSrHqnHqQwQSq
mLMZRMRmZLmHLLjNshrrJjRsNNsj
PnqnBVBPcpWrWfWzDhDsHW
qpBQgcQncqpBcVgtclHqcSMTLZZQvLLZdLdwCmLMSd
PZgMwZMjPgBCWgZFgmBGSTtwcccztTzbnGDcNT
vJLlvvlfdsVVslVHrrpLfRlDcSScmbSnnRGDnbDTSbSSDt
fvVHdLfLQljmmqqMQPhQ
DZRstNGrDWGRMlzLHLMpNSpp
bPgCrCvgnrCPgwplbMSjfHzHfj
CgnFnBVrFFVmdVggmPPVTTcZBGhRqGtcZqqJhGRGssWs
WrrCGrDlWhBBZtHB
ggdcnQpcPRSpzfvJBTfPJvtP
gSNnSBFnSMBNjpMssDmCrrGmCLqq
szZGZGGwNdVtpwvbzptzVBQQLFclTJQFlHJNcFBRRJ
CTrqSfWThWDjJCcLQRRRFLlQ
rggqfMqPWnDqnVbZvTtPZpssVw
GJHqPPcJnhthNHnL
rzzlVDVTQDzSSdsSlzpBspssLhmZNjhZWjjffWBLZhgBmnjt
TSTQTsnTbDsMvCGwcFvbJR
PPjtzdnCnSjSthPjWJgJWjgMNBsNrT
bZHfLLhvhpbfvLhHfQbqpNJTWWNsqNTrTNrgWN
ZFbZZFvlRvZwhQwLDSdGnmCGmnlmdznD
RFRRhzzsRsszRlhbrhgBcbTfHgBT
pJtNmSgwZNtSJLgSqHcbBDDTHPfrSqTc
ZNWdpNCJtLZpQJLLpmLtZNFGjVgFnsngRGllVsjjCnvs
sJVRJmmmwwlmzcszMzjRjJVzspCZBsZTTFpTTZvSZTFBFbtZ
GhNDgNWnGGqDvFSCZvbtFpvg
fWrCNNffGnhWdrPhWjwjzRlJmcjRcPzlmV
FdncmqmgcZztLWFvFFvjWLlT
RsMMJVpfhJVsDVMfJpvlTWTvhjQSQvCbLCSC
jDBjRrsVfNJgzdZrZwtmZt
THDDQLZTGQQLQDSSTBCZSZHfFrvvlVRRlPVFfVrvqnvnnH
VbWWcgbNpjgPrRvrNfqnzv
swggwpJWwsWscJsDSLTJTmSVZJTBDZ
wGsznPGGBHdzHhtHDD
WMVSWqVmrSBMpvWVMFtdClhDtqDDHhdtQt
RgWVRcmgnPPnjJBg
BmfPBHBFswLLjQQnLCnzzW
SRdvvRSGrdlRSvTgRrMVCjVWCZzMnVZhWzTj
vlSRJrRJvbGGrSSJgWvlScptFmwbmfHHBfsNFmBbfsFm"""
