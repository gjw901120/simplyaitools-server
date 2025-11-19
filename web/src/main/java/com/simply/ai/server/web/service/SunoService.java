package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;

public interface SunoService {

    void sunoGenerate(SunoGenerateDTO request);

    void sunoExtend(SunoExtendDTO request);

    void sunoUploadCover(SunoUploadCoverDTO request);

    void sunoAddVocal(SunoAddVocalsDTO request);

    void sunoUploadExtend(SunoUploadExtendDTO request);

    void sunoAddInstrumental(SunoAddInstrumentalDTO request);

}
