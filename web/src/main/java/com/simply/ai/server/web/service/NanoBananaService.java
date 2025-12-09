package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.image.NanoBananaEditDTO;
import com.simply.ai.server.web.model.dto.request.image.NanoBananaGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface NanoBananaService {

    BaseResponse nanoBananaGenerate(NanoBananaGenerateDTO request);

    BaseResponse nanoBananaEdit(NanoBananaEditDTO request);

}
