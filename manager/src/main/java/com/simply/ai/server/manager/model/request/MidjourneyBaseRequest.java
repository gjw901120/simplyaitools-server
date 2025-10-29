// MidjourneyBaseRequest.java
package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.enums.MidjourneyBotTypeEnum;
import com.simply.ai.server.manager.enums.MidjourneyModeEnum;
import lombok.Data;
import org.hibernate.validator.constraints.URL;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * Midjourney 基础请求参数
 */
@Data
public class MidjourneyBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Bot 类型
     */
    @NotNull(message = "Bot类型不能为空")
    private MidjourneyBotTypeEnum botType = MidjourneyBotTypeEnum.MID_JOURNEY;

    /**
     * 账号过滤器
     */
    private AccountFilter accountFilter;

    /**
     * 回调地址
     */
    @URL(message = "回调地址格式不正确")
    private String notifyHook;

    /**
     * 自定义状态参数
     */
    private String state;

    /**
     * 账号过滤器（内部类）
     */
    @Data
    public static class AccountFilter implements Serializable {

        private static final long serialVersionUID = 1L;

        /**
         * 频道 ID
         */
        private String channelId;

        /**
         * 账号实例 ID
         */
        private String instanceId;

        /**
         * 账号模式数组
         */
        private List<Mode> modes;

        /**
         * 备注包含的内容
         */
        private String remark;

        /**
         * 账号是否支持 remix
         */
        private Boolean remix = true;

        /**
         * remix 自动提交设置
         */
        private Boolean remixAutoConsidered = true;

        /**
         * 账号模式枚举（内部枚举）
         */
        public enum Mode {
            RELAX("RELAX", "放松模式"),
            FAST("FAST", "快速模式"),
            TURBO("TURBO", "涡轮模式");

            private final String code;
            private final String description;

            Mode(String code, String description) {
                this.code = code;
                this.description = description;
            }

            public String getCode() {
                return code;
            }

            public String getDescription() {
                return description;
            }

            public static Mode getByCode(String code) {
                for (Mode value : values()) {
                    if (value.getCode().equals(code)) {
                        return value;
                    }
                }
                return null;
            }
        }

        /**
         * 构建方法
         */
        public static AccountFilter create() {
            return new AccountFilter();
        }

        /**
         * 设置频道 ID
         */
        public AccountFilter withChannelId(String channelId) {
            this.channelId = channelId;
            return this;
        }

        /**
         * 设置实例 ID
         */
        public AccountFilter withInstanceId(String instanceId) {
            this.instanceId = instanceId;
            return this;
        }

        /**
         * 设置模式
         */
        public AccountFilter withModes(List<Mode> modes) {
            this.modes = modes;
            return this;
        }

        /**
         * 设置单个模式
         */
        public AccountFilter withMode(Mode mode) {
            if (this.modes == null) {
                this.modes = new java.util.ArrayList<>();
            }
            this.modes.add(mode);
            return this;
        }

        /**
         * 设置备注
         */
        public AccountFilter withRemark(String remark) {
            this.remark = remark;
            return this;
        }

        /**
         * 设置 remix
         */
        public AccountFilter withRemix(Boolean remix) {
            this.remix = remix;
            return this;
        }

        /**
         * 设置 remix 自动提交
         */
        public AccountFilter withRemixAutoConsidered(Boolean remixAutoConsidered) {
            this.remixAutoConsidered = remixAutoConsidered;
            return this;
        }
    }

    /**
     * 构建基础请求
     */
    public static MidjourneyBaseRequest base() {
        return new MidjourneyBaseRequest();
    }

    /**
     * 设置 Bot 类型
     */
    public <T extends MidjourneyBaseRequest> T withBotType(MidjourneyBotTypeEnum botType) {
        this.botType = botType;
        return (T) this;
    }

    /**
     * 设置回调地址
     */
    public <T extends MidjourneyBaseRequest> T withNotifyHook(String notifyHook) {
        this.notifyHook = notifyHook;
        return (T) this;
    }

    /**
     * 设置状态参数
     */
    public <T extends MidjourneyBaseRequest> T withState(String state) {
        this.state = state;
        return (T) this;
    }

    /**
     * 设置账号过滤器
     */
    public <T extends MidjourneyBaseRequest> T withAccountFilter(AccountFilter accountFilter) {
        this.accountFilter = accountFilter;
        return (T) this;
    }

    /**
     * 快速设置频道 ID
     */
    public <T extends MidjourneyBaseRequest> T withChannelId(String channelId) {
        if (this.accountFilter == null) {
            this.accountFilter = new AccountFilter();
        }
        this.accountFilter.setChannelId(channelId);
        return (T) this;
    }

    /**
     * 快速设置实例 ID
     */
    public <T extends MidjourneyBaseRequest> T withInstanceId(String instanceId) {
        if (this.accountFilter == null) {
            this.accountFilter = new AccountFilter();
        }
        this.accountFilter.setInstanceId(instanceId);
        return (T) this;
    }

    /**
     * 快速设置备注
     */
    public <T extends MidjourneyBaseRequest> T withRemark(String remark) {
        if (this.accountFilter == null) {
            this.accountFilter = new AccountFilter();
        }
        this.accountFilter.setRemark(remark);
        return (T) this;
    }

    /**
     * 快速设置模式
     */
    public <T extends MidjourneyBaseRequest> T withMode(AccountFilter.Mode mode) {
        if (this.accountFilter == null) {
            this.accountFilter = new AccountFilter();
        }
        if (this.accountFilter.getModes() == null) {
            this.accountFilter.setModes(new java.util.ArrayList<>());
        }
        this.accountFilter.getModes().add(mode);
        return (T) this;
    }
}