package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface SunoService {

    BaseResponse sunoGenerate(SunoGenerateDTO request);

    BaseResponse sunoExtend(SunoExtendDTO request);

    BaseResponse sunoUploadCover(SunoUploadCoverDTO request);

    BaseResponse sunoAddVocal(SunoAddVocalsDTO request);

    BaseResponse sunoUploadExtend(SunoUploadExtendDTO request);

    BaseResponse sunoAddInstrumental(SunoAddInstrumentalDTO request);

}
