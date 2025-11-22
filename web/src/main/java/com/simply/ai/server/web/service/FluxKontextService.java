package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.FluxKontextGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface FluxKontextService {

    BaseResponse fluxKontextGenerate(FluxKontextGenerateDTO request);

}
